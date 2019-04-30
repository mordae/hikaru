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

module Simple (main)
where
  import Control.Concurrent.MVar
  import Control.Monad.Reader
  import Data.Text (Text)
  import Lucid
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Network.Wai.Middleware.RequestLogger
  import Web.Hikaru


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

      -- Enable 300s cache for the static endpoints.
      wrapAction (defaultHeader hCacheControl "public, max-age=300" >>) $ do
        -- Negotiate content for the root page.
        route $ getRootHtmlR <$ get <* offerHTML
        route $ getRootTextR <$ get <* offerText

      -- Disable caching for all the following endpoints.
      wrapActions (defaultHeader hCacheControl "no-store" >>)

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
