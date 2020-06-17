{-|
Module      :  Hikaru.Demo
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)
-}

module Hikaru.Demo
  ( makeDemo
  )
where
  import BasePrelude hiding (for_, Option, Control)

  import Control.Monad.Reader
  import Data.Aeson ()
  import Data.Text (Text)
  import Hikaru
  import Lucid
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import Network.Wai


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
      modifyMVar_ counter (return . succ)
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
    respond $ \ae -> do
      runReaderT (unAction act) (DemoEnv ae me)


  makeApplication :: ModelEnv -> Application
  makeApplication me = do
    dispatch (runAction me) do
      -- Register nicer 404 error handler.
      handler NotFound handleNotFound

      -- Negotiate content for the root page.
      route $ getRootHtmlR <$ get <* offerHTML
      route $ getRootTextR <$ get <* offerText

      -- Disable caching for all the following endpoints.
      wrapAction (defaultHeader hCacheControl "no-store" >>) do
        -- Present a simple greeting page.
        route $ getHelloR <$ get <* seg "hello" <*> arg
                          <* offerText

        -- Present an echoing JSON API.
        route $ postEchoR <$ post <* seg "api" <* seg "echo"
                          <* offerJSON <* acceptJSON

        -- Case handling.
        route $ postCaseR <$ post <* seg "case" <* seg "" <* acceptForm
        route $ getCasesR <$ get  <* seg "case" <* seg "" <* offerJSON


  -- Handlers ----------------------------------------------------------------


  getRootHtmlR :: Action ()
  getRootHtmlR = do
    -- Update the counter.
    n <- countVisitor

    -- Present fancy HTML result.
    sendHTML do
      h1_ "Welcome!"
      p_ $ "You are " >> toHtml (show n) >> ". visitor!"


  getRootTextR :: Action ()
  getRootTextR = do
    -- Update the counter.
    n <- countVisitor

    -- Present a plain textual result.
    sendString $ unlines [ "Welcome!"
                         , "You are " <> show n <> ". visitor!"
                         ]


  postEchoR :: Action ()
  postEchoR = sendJSON =<< getJSON


  getHelloR :: Text -> Action ()
  getHelloR name = sendText $ "Hello, " <> name <> "!"


  handleNotFound :: RequestError -> Text -> Action ()
  handleNotFound _exn msg = do
    setStatus status404
    sendHTML do
      h1_ "404 Not Found"
      p_ (toHtml msg)


  postCaseR :: Action ()
  postCaseR = do
    (res, view) <- postForm "addCase" addCaseForm

    case res of
      Nothing -> do
        setStatus status400
        sendHTML do
          simpleForm_ view

      Just ac -> do
        _case <- addCase ac
        redirect "/case/"


  getCasesR :: Action ()
  getCasesR = do
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

        for cases \Case{..} -> do
          tr_ do
            td_ $ toHtml $ show caseId
            td_ $ toHtml $ caseName
            td_ $ toHtml $ caseRecNo
            td_ $ toHtml $ show caseMode
            td_ $ toHtml $ show caseActive


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
          forM_ elemControls \ctrl@Control{..} -> do
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