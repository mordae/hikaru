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
  import Lucid
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
          h1_ "Welcome!"
          p_ $ "You are " >> toHtml (tshow n) >> ". visitor!"


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
    sendText $ "See: " <> rhref getRootHtmlR [("q", "404")]


  postCaseR :: Route '[] Handler
  postCaseR = post handle // "case" // "" /? acceptForm
    where
      handle = do
        setLanguages ["en", "cs"]

        (res, form) <- postForm addCaseForm

        case res of
          Nothing -> do
            setStatus status400
            sendHTML do
              simpleForm_ form

          Just ac -> do
            _case <- addCase ac
            redirect (rhref getCasesR [])


  getCasesR :: Route '[] Handler
  getCasesR = get handle // "case" // "" /? offerHTML
    where
      handle = do
        setHeader hCacheControl "no-store"
        setLanguages ["en", "cs"]

        cases <- liftIO . readMVar . (.modelCases) =<< getModelEnv

        sendHTML do
          h1_ "Cases"

          form_ [method_ "POST"] do
            form <- newForm addCaseForm
            simpleForm_ form
            button_ [type_ "submit"] "Submit"

          table_ do
            tr_ do
              th_ "Id"
              th_ "Name"
              th_ "RecNo"
              th_ "Mode"
              th_ "Active"

            forM cases \Case{..} -> do
              tr_ do
                td_ $ toHtml $ tshow caseId
                td_ $ toHtml $ caseName
                td_ $ toHtml $ caseRecNo
                td_ $ toHtml $ tshow caseMode
                td_ $ toHtml $ tshow caseActive


  -- Forms -------------------------------------------------------------------


  simpleForm_ :: (MonadAction m, Localizable l) => Form l -> HtmlT m ()
  simpleForm_ Form{..} = do
    forM_ formControls \ctrl@Control{..} -> do
      div_ do
        div_ do
          case ctrlLabel of
            Nothing -> return ()
            Just lb -> label_ [for_ ctrlName] $ lc_ lb

        formControl_ ctrl

        forM_ ctrlNotes \Note{..} -> do
          p_ do
            lc_ noteMessage


  formControl_ :: (Localizable l, MonadAction m) => Control l -> HtmlT m ()
  formControl_ Control{..} = do
    case ctrlType of
      "select" -> do
        select_ [name_ ctrlName, id_ ctrlName] do
          mapM_ (choice_ $ fromMaybe [] ctrlValues) ctrlChoices

      "multiselect" -> do
        select_ [name_ ctrlName, id_ ctrlName, multiple_ "multiple"] do
          mapM_ (choice_ $ fromMaybe [] ctrlValues) ctrlChoices

      "textarea" -> do
        textarea_ [name_ ctrlName, id_ ctrlName] do
          case ctrlValues of
            Just (text:_) -> toHtml text
            _otherwise    -> ""

      _other -> do
        ph <- lc ctrlPlaceholder

        input_ [ type_ ctrlType
               , name_ ctrlName
               , placeholder_ ph
               , value_ case ctrlValues of
                          Just (text:_) -> text
                          _otherwise    -> ""
               ]


  choice_ :: (MonadAction m, Localizable l) => [Text] -> (l, Text) -> HtmlT m ()
  choice_ selected (l, v) = do
    if v `elem` selected
       then option_ [selected_ "selected", value_ v] $ lc_ l
       else option_ [value_ v] $ lc_ l


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
    localize "en" MsgCaseName    = Just "Name"
    localize "en" MsgCaseMode    = Just "Mode"
    localize "en" MsgCaseEnabled = Just "Enabled"
    localize "en" MsgCaseRecNo   = Just "Record #"
    localize "en" MsgSubmit      = Just "Submit"

    localize "en" MsgRequired    = Just "This field is required."

    localize "en" (MsgAccessMode ModePublic)  = Just "Public"
    localize "en" (MsgAccessMode ModePrivate) = Just "Private"

    localize "en" (MsgBool True)  = Just "True"
    localize "en" (MsgBool False) = Just "False"

    localize _lang _msg          = Nothing


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

    return $ AddCase <$> acName <*> acRecNo <*> acMode <*> acActive


-- vim:set ft=haskell sw=2 ts=2 et:
