-- |
-- Module      :  Hikaru.Demo
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Hikaru.Demo
  ( makeDemo
  )
where
  import Praha hiding (for_)

  import Hikaru

  import Data.Aeson (Value)
  import Data.Text (unlines)
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import UnliftIO.MVar


  -- Action ------------------------------------------------------------------


  -- |
  -- Our custom action monad allows us to inspect request,
  -- build response and consult the model at the same time.
  --
  newtype Action a
    = Action
      { unAction         :: ReaderT DemoEnv IO a
      }
    deriving (Functor, Applicative, Monad, MonadIO)

  instance MonadAction Action where
    askActionEnv = Action ((.demoActionEnv) <$> ask)

  instance MonadModel Action where
    getModelEnv = Action ((.demoModelEnv) <$> ask)


  data DemoEnv
    = DemoEnv
      { demoActionEnv  :: ActionEnv
      , demoModelEnv   :: ModelEnv
      }

  type Handler = Action ()


  -- Model -------------------------------------------------------------------


  class (MonadIO m) => MonadModel m where
    getModelEnv :: m ModelEnv


  data ModelEnv
    = ModelEnv
      { modelCounter   :: MVar Word
      , modelCases     :: MVar [Case]
      }


  makeModelEnv :: Word -> IO ModelEnv
  makeModelEnv n = ModelEnv <$> newMVar n <*> newMVar []


  countVisitor :: (MonadModel m) => m Word
  countVisitor = do
    counter <- (.modelCounter) <$> getModelEnv
    liftIO do
      modifyMVar_ counter (return . (+ 1))
      readMVar counter


  addCase :: (MonadModel m) => AddCase -> m Case
  addCase AddCase{..} = do
    nextId  <- liftIO . readMVar . (.modelCounter) =<< getModelEnv

    let case' = Case { caseId     = nextId
                     , caseName   = acName
                     , caseRecNo  = fromMaybe acName acRecNo
                     , caseMode   = acMode
                     , caseActive = acActive
                     }

    cases <- (.modelCases) <$> getModelEnv

    liftIO do
      modifyMVar_ cases (return . (<> [case']))
      return case'


  -- Dispatching -------------------------------------------------------------


  makeDemo :: IO Application
  makeDemo = do
    model <- makeModelEnv 0
    return $ makeApplication model


  runAction :: ModelEnv -> Action () -> Application
  runAction me act = do
    respond \ae -> do
      runReaderT ((.unAction) act) (DemoEnv ae me)


  makeApplication :: ModelEnv -> Application
  makeApplication me = do
    dispatch (runAction me) do
      -- Register nicer 404 error handler.
      handler 404 notFound

      -- Negotiate content for the root page.
      route getRootHtmlR
      route getRootTextR

      -- Present a simple greeting page.
      route getHelloR

      -- Present an echoing JSON API.
      route postEchoR

      -- Handle new cases.
      route postCaseR

      -- Handle case listing.
      route getCasesR


  -- Routes ------------------------------------------------------------------


  getRootHtmlR :: Route '[] Handler
  getRootHtmlR = get handle /? offerHTML
    where
      handle = do
        -- Update the counter.
        n <- countVisitor

        -- Present fancy HTML result.
        sendHTML do
          tag "h1" "" do
            text "Welcome!"

          tag "p" "" do
            text $ mconcat [ "You are ", tshow n, ". visitor!" ]


  getRootTextR :: Route '[] Handler
  getRootTextR = get handle /? offerText
    where
      handle = do
        -- Update the counter.
        n <- countVisitor

        -- Present a plain textual result.
        sendText $ unlines [ "Welcome!"
                           , "You are " <> tshow n <> ". visitor!"
                           ]


  postEchoR :: Route '[] Handler
  postEchoR = post handle // "api" // "echo" /? offerJSON /? acceptJSON
    where
      handle = do
        setHeader hCacheControl "no-store"
        (json :: Value) <- getJSON
        sendJSON json


  getHelloR :: Route '[Text] Handler
  getHelloR = get handle // "hello" /: "name" /? offerText
    where
      handle name = do
        setHeader hCacheControl "no-store"

        when (name == "nobody") do
          abort badRequest400 [] "I don't like you."

        sendText $ "Hello, " <> name <> "!"


  notFound :: Response -> Action ()
  notFound _resp = do
    setStatus status404
    sendText $ "See: " <> buildLink getRootHtmlR [("q", "404")]


  postCaseR :: Route '[] Handler
  postCaseR = post handle // "case" // "" /? acceptForm
    where
      handle = do
        selectLanguage "lang" "lang" "en"

        (res, form) <- postForm addCaseForm

        case res of
          Nothing -> do
            setStatus status400
            sendHTML do
              simpleForm form

          Just ac -> do
            _case <- addCase ac
            redirect (buildLink getCasesR [])


  getCasesR :: Route '[] Handler
  getCasesR = get handle // "case" // "" /? offerHTML
    where
      handle = do
        selectLanguage "lang" "lang" "en"

        cases <- liftIO . readMVar . (.modelCases) =<< getModelEnv

        sendHTML do
          tag "h1" "" do
            text "Cases"

          tag "form" "" do
            attr [ "method" .= "POST" ]

            form <- newForm addCaseForm
            simpleForm form

          tag "table" "" do
            tag "tr" "" do
              tag "th" "" $ text "Id"
              tag "th" "" $ text "Name"
              tag "th" "" $ text "RecNo"
              tag "th" "" $ text "Mode"
              tag "th" "" $ text "Active"

            forM cases \Case{..} -> do
              tag "tr" "" do
                tag "td" "" $ text $ tshow caseId
                tag "td" "" $ text $ caseName
                tag "td" "" $ text $ caseRecNo
                tag "td" "" $ text $ tshow caseMode
                tag "td" "" $ text $ tshow caseActive


  -- Forms -------------------------------------------------------------------


  simpleForm :: (MonadAction m, Localizable l) => Form l -> HtmlT m ()
  simpleForm Form{..} = do
    forM_ formControls \ctrl@Control{..} -> do
      tag "div" "" do
        tag "div" "" do
          case ctrlLabel of
            Nothing -> pass
            Just lb -> tag "label" "" do
              attr [ "for" .= ctrlName ]
              localize' lb

        formControl ctrl

        forM_ ctrlNote \Note{..} -> do
          tag "p" "" do
            localize' noteMessage


  formControl :: (Localizable l, MonadAction m) => Control l -> HtmlT m ()
  formControl Control{..} = do
    case ctrlType of
      "select" -> do
        tag "select" "" do
          attr [ "name" .= ctrlName
               , "id"   .= ctrlName
               ]
          mapM_ (choice $ fromMaybe [] ctrlValues) ctrlChoices

      "multiselect" -> do
        tag "select" "" do
          attr [ "name"     .= ctrlName
               , "id"       .= ctrlName
               , "multiple" .= "multiple"
               ]
          mapM_ (choice $ fromMaybe [] ctrlValues) ctrlChoices

      "textarea" -> do
        tag "textarea" "" do
          attr [ "name" .= ctrlName
               , "id"   .= ctrlName
               ]

          case ctrlValues of
            Just (t:_) -> text t
            _otherwise -> pass

      _other -> do
        ph <- localizeText' ctrlPlaceholder

        tag "input" "" do
          attr [ "type"        .= ctrlType
               , "name"        .= ctrlName
               , "id"          .= ctrlName
               , "placeholder" .= ph
               , "value"       .= case ctrlValues of
                                    Just (t:_) -> t
                                    _otherwise -> ""
               ]


  choice :: (MonadAction m, Localizable l) => [Text] -> (l, Text) -> HtmlT m ()
  choice selected (l, v) = do
    tag "option" "" do
      attr [ "value" .= v ]

      when (v `elem` selected) do
        attr [ "selected" .= "" ]

      localize' l


  data Case
    = Case
      { caseId         :: Word
      , caseName       :: Text
      , caseRecNo      :: Text
      , caseMode       :: AccessMode
      , caseActive     :: Bool
      }
    deriving (Show, Generic)


  data AddCase
    = AddCase
      { acName         :: Text
      , acRecNo        :: Maybe Text
      , acMode         :: AccessMode
      , acActive       :: Bool
      }

  data AccessMode
    = ModePublic
    | ModePrivate
    deriving (Show, Enum, Bounded, Eq)

  instance Param AccessMode where
    toParam ModePublic  = "public"
    toParam ModePrivate = "private"

    fromParam "public"  = Just ModePublic
    fromParam "private" = Just ModePrivate
    fromParam _else     = Nothing


  data RenderHint
    = RenderCollapsed
    | RenderExpanded
    deriving (Eq, Show)


  data Messages
    = MsgCaseName
    | MsgCaseRecNo
    | MsgCaseMode
    | MsgCaseEnabled
    | MsgRequired
    | MsgSubmit
    | MsgAccessMode AccessMode
    | MsgBool Bool
    deriving (Show)

  instance Localizable Messages where
    localize "en" msg = english msg
    localize ____ msg = english msg


  english :: (MonadIO m) => Messages -> HtmlT m ()
  english MsgCaseName    = text "Name"
  english MsgCaseMode    = text "Mode"
  english MsgCaseEnabled = text "Enabled"
  english MsgCaseRecNo   = text "Record #"
  english MsgSubmit      = text "Submit"

  english MsgRequired    = text "This field is required."

  english (MsgAccessMode ModePublic)  = text "Public"
  english (MsgAccessMode ModePrivate) = text "Private"

  english (MsgBool True)  = text "True"
  english (MsgBool False) = text "False"


  addCaseForm :: (MonadAction m) => FormT Messages m (Maybe AddCase)
  addCaseForm = do
    acName <- input "name" MsgCaseName Nothing
                [ required MsgRequired
                ]

    acRecNo <- input "recno" MsgCaseRecNo Nothing
                [ required MsgRequired
                , format "number"
                ]

    acMode <- select "mode" MsgCaseMode Nothing
                [ choicesEnum MsgAccessMode
                , required MsgRequired
                ]

    acActive <- select "active" MsgCaseEnabled Nothing
                  [ choicesEnum MsgBool
                  , required MsgRequired
                  ]

    _submit <- button "submit" MsgSubmit []

    sameOrigin <- checkSameOrigin

    if sameOrigin
       then return $ AddCase <$> acName <*> acRecNo <*> acMode <*> acActive
       else return $ Nothing


-- vim:set ft=haskell sw=2 ts=2 et:
