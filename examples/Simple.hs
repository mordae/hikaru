{-|
Module      :  Simple
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

Demonstration of a simple stateful web service built using Hikaru.

Simple /= Easy /= Short. Happy reading.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Simple (main)
where
  import Prelude
  import Control.Concurrent.MVar
  import Control.Monad.Reader
  import Data.Maybe
  import Data.Text (Text)
  import Hikaru
  import Lucid
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Network.Wai.Middleware.RequestLogger


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
      }


  makeModelEnv :: Word -> IO ModelEnv
  makeModelEnv n = ModelEnv <$> newMVar n


  countVisitor :: (MonadModel m) => m Word
  countVisitor = do
    counter <- modelCounter <$> getModelEnv
    liftIO $ do
      modifyMVar_ counter (return . succ)
      readMVar counter


  -- Dispatching -------------------------------------------------------------


  runAction :: ModelEnv -> Action () -> Application
  runAction me act = do
    respond $ \ae -> do
      runReaderT (unAction act) (DemoEnv ae me)


  makeApplication :: ModelEnv -> Application
  makeApplication me = do
    dispatch (runAction me) $ do
      -- Register nicer error handlers.
      handler NotFound handleNotFound

      -- Plug in a cool logging middleware.
      middleware $ logStdoutDev

      -- Negotiate content for the root page.
      route $ getRootHtmlR <$ get <* offerHTML
      route $ getRootTextR <$ get <* offerText

      -- Disable caching for all the following endpoints.
      wrapAction (defaultHeader hCacheControl "no-store" >>) $ do
        -- Return search results and repeat the form.
        route $ getSearchHtmlR <$ get <* seg "search"
                               <* offerHTML

        -- Present a simple greeting page.
        route $ getHelloR <$ get <* seg "hello" <*> arg
                          <* offerText

        -- Create an echoing JSON API.
        route $ postEchoR <$ post <* seg "api" <* seg "echo"
                          <* offerJSON <* acceptJSON


  -- Handlers ----------------------------------------------------------------


  getRootHtmlR :: Action ()
  getRootHtmlR = do
    -- Update the counter.
    n <- countVisitor

    -- Present fancy HTML result.
    sendHTML $ do
      h1_ "Welcome!"
      p_ $ "You are " >> toHtml (show n) >> ". visitor!"

      form_ [action_ "/search", method_ "GET"] $ do
        view <- newForm "search" $ searchForm (Just "meaning of life")
        formView_ view


  getSearchHtmlR :: Action ()
  getSearchHtmlR = do
    sendHTML $ do
      (maybeQuery, view) <- getForm "search" (searchForm Nothing)

      h1_ "Search results"
      form_ [method_ "GET"] $ do
        formView_ view

      case maybeQuery of
        Nothing -> ""
        Just q -> do
          hr_ []
          h2_ $ toHtml q
          p_ "Sorry, no results found!"


  searchForm :: (Monad m) => Maybe Text -> FormT Text m (Maybe Text)
  searchForm q = do
     q' <- inputField "q" "Query" q
     _  <- button "search" "Search"
     return q'


  formView_ :: (MonadAction m, Localized l) => FormView l -> HtmlT m ()
  formView_ view = do
    forM_ (formElements view) $ \element ->
      case element of
        Button{..} -> do
          button_ [ id_ elemName
                  , name_ elemName
                  ] $ lc_ elemLabel

        InputField{..} -> do
          label_ [ for_ elemName ] $ do
            lc_ elemLabel
            ":"

          input_ [ id_ elemName
                 , name_ elemName
                 , value_ (fromMaybe "" elemValue)
                 ]


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
    sendHTML $ do
      h1_ "404 Not Found"
      p_ (toHtml msg)


  -- Serving -----------------------------------------------------------------


  main :: IO ()
  main = do
    putStrLn "Listening (port 5000) ..."
    model <- makeModelEnv 0
    run 5000 (makeApplication model)


-- vim:set ft=haskell sw=2 ts=2 et:
