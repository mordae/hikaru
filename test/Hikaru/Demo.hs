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

  import UnliftIO.MVar
  import Data.Aeson (Value)
  import Lucid
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import Network.Wai
  import Data.Text (unlines)


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
    getActionEnv = Action (demoActionEnv <$> ask)

  instance MonadModel Action where
    getModelEnv = Action (demoModelEnv <$> ask)


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
    counter <- modelCounter <$> getModelEnv
    liftIO do
      modifyMVar_ counter (return . (+ 1))
      readMVar counter


  addCase :: (MonadModel m) => AddCase -> m Case
  addCase AddCase{..} = do
    nextId  <- liftIO . readMVar . modelCounter =<< getModelEnv

    let case' = Case { caseId     = nextId
                     , caseName   = acName
                     , caseRecNo  = fromMaybe acName acRecNo
                     , caseMode   = acMode
                     , caseActive = acActive
                     }

    cases <- modelCases <$> getModelEnv

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
      runReaderT (unAction act) (DemoEnv ae me)


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
        (res, view) <- postForm "addCase" addCaseForm

        case res of
          Nothing -> do
            setStatus status400
            sendHTML do
              simpleForm_ view

          Just ac -> do
            _case <- addCase ac
            redirect (rhref getCasesR [])


  getCasesR :: Route '[] Handler
  getCasesR = get handle // "case" // "" /? offerHTML
    where
      handle = do
        setHeader hCacheControl "no-store"

        cases <- liftIO . readMVar . modelCases =<< getModelEnv

        sendHTML do
          h1_ "Cases"

          form_ [method_ "POST"] do
            view <- newForm "addCase" Nothing addCaseForm
            simpleForm_ view
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


  simpleForm_ :: (MonadAction m, Localizable l) => View l -> HtmlT m ()
  simpleForm_ View{..} = do
    forM_ viewControls \ctrl@Control{..} -> do
      viewControl_ ctrl
      forM_ ctrlNotes \Note{..} -> do
        p_ do
          lc_ noteMessage

    forM_ viewElements \Element{..} -> do
      div_ do
        div_ do
          case elemControls of
            Control{..}:_ -> label_ [for_ ctrlName] $ lc_ elemLabel
            _otherwise    -> label_ $ lc_ elemLabel

        div_ do
          forM_ elemControls \ctrl@Control{} -> do
            viewControl_ ctrl

          forM_ elemControls \Control{..} -> do
            forM_ ctrlNotes \Note{..} -> do
              p_ do
                lc_ noteMessage


  viewControl_ :: (Localizable l, MonadAction m) => Control l -> HtmlT m ()
  viewControl_ Control{..} = do
    case ctrlField of
      InputField{..} -> do
        ph <- lc fieldPlacehold
        input_ [ type_ fieldType
               , name_ ctrlName
               , placeholder_ ph
               , value_ fieldValue
               ]

      SelectField{..} -> do
        select_ [name_ ctrlName] do
          mapM_ viewOption_ fieldOptions


  viewOption_ :: (MonadAction m, Localizable l) => Option l -> HtmlT m ()
  viewOption_ Option{..} = do
    case optionSelected of
      True -> do
        option_ [selected_ "selected", value_ optionValue] do
          lc_ optionLabel

      False -> do
        option_ [value_ optionValue] do
          lc_ optionLabel


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
    deriving (Show, Eq)

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
    | MsgCaseMode
    | MsgCaseEnabled
    | MsgForm FormMessage
    deriving (Show)

  instance Localizable Messages where
    localize "en" MsgCaseName    = Just "Name"
    localize "en" MsgCaseMode    = Just "Mode"
    localize "en" MsgCaseEnabled = Just "Enabled"

    localize lang (MsgForm msg)  = localize lang msg
    localize _lang _msg          = Nothing

  instance FromFormMessage Messages where
    fromFormMessage = MsgForm


  addCaseForm :: (MonadAction m) => Form Messages m AddCase
  addCaseForm = do
    (\(x1, x2) x3 x4 -> AddCase x1 x2 x3 x4)
      <$> element MsgCaseName do
            (,)
              <$> input "name" acName do
                    return ()

              <*> input "recno" acRecNo do
                    return ()

      <*> element MsgCaseMode do
            select "mode" acMode do
              return ()

      <*> element MsgCaseEnabled do
            select "active" acActive do
              hint RenderExpanded


-- vim:set ft=haskell sw=2 ts=2 et:
