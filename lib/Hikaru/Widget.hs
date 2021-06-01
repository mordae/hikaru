-- |
-- Module      :  Hikaru.Widget
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module tries to simplify building custom HTML widgets that react
-- to query string parameters and/or form fields and render with different
-- CSS frameworks such as Bootstrap or Bulma.
--
-- The primary user is the "Hikaru.Form" module, but you are welcome to
-- used it for any interactive content such as the filtered, sorted and
-- paged tables all applications seem to eventually grow.
--
-- Example:
--
-- @
-- -- Alias for Bootstrap 5 in our case.
-- data B5 = B5
--
-- -- Base type for our widget.
-- data RedText = RedText (Maybe Text)
--
-- -- Nothing fancy, just render a piece of text in red.
-- instance (Monad m) => 'Render' B5 m RedText where
--   'render' (RedText Nothing) = \"\"
--   'render' (RedText (Just text)) = do
--     span_ [style_ \"color: red\"] do
--       'toHtml' text
--
-- -- Smart constructor that can take a default.
-- redText :: (Monad m) => Maybe Text -> 'WidgetT' m RedText
-- redText dfl = do
--   text <- 'getArgumentMaybe' "text"
--   return $ RedText $ text \<|\> dfl
--
-- -- And this is how you use it.
-- demo :: ('Hikaru.Action.MonadAction' m) => 'HtmlT' m ()
-- demo = do
--   'getWidget' @B5 \"demo.\" do
--     redText (Just \"Hello, World!\")
-- @
--

module Hikaru.Widget
  (
  -- * Using Widgets
    newWidget
  , getWidget
  , postWidget

  -- * Building Widgets
  , WidgetT
  , getArguments
  , getArgumentMaybe
  , getArgumentDefault
  , getArgumentList

  -- * Rendering Widgets
  , Render(..)
  )
where
  import Praha
  import Data.Text (stripPrefix)
  import Data.List (lookup, filter)
  import Lucid
  import Hikaru.Types
  import Hikaru.Action


  -- |
  -- Create new widget without GET or POST data.
  --
  newWidget :: forall t m a. (Render t m a, MonadAction m)
            => Text -> WidgetT m a -> HtmlT m ()
  newWidget prefix WidgetT{..} = do
    widget <- lift $ runReaderT runWidgetT =<< newEnv prefix
    render @t widget


  -- |
  -- Create the widget using GET query string parameters.
  --
  getWidget :: forall t m a. (Render t m a, MonadAction m)
            => Text -> WidgetT m a -> HtmlT m ()
  getWidget prefix WidgetT{..} = do
    widget <- lift $ runReaderT runWidgetT =<< getEnv prefix
    render @t widget


  -- |
  -- Create the widget using POST form fields.
  --
  postWidget :: forall t m a. (Render t m a, MonadAction m)
             => Text -> WidgetT m a -> HtmlT m ()
  postWidget prefix WidgetT{..} = do
    widget <- lift $ runReaderT runWidgetT =<< postEnv prefix
    render @t widget


  -- |
  -- Create widget environment without GET or POST data.
  --
  newEnv :: (MonadAction m) => Text -> m Env
  newEnv prefix = do
    args <- pure []
    return Env{..}


  -- |
  -- Create widget environment from GET query string parameters.
  --
  getEnv :: (MonadAction m) => Text -> m Env
  getEnv prefix = do
    args <- mapMaybe (unprefix prefix) <$> getParams
    return Env{..}


  -- |
  -- Create widget environment from POST form fields.
  --
  postEnv :: (MonadAction m) => Text -> m Env
  postEnv prefix = do
    args <- mapMaybe (unprefix prefix) <$> getFields
    return Env{..}


  -- |
  -- Return only keys matching given prefix while stripping it.
  --
  unprefix :: Text -> (Text, v) -> Maybe (Text, v)
  unprefix prefix (key, val) = (, val) <$> stripPrefix prefix key


  -- |
  -- Context for widget construction.
  --
  -- Makes it possible to access widget arguments in a way that is
  -- portable across request methods and scoped, so that the same widget
  -- type can be used multiple times on a single page.
  --
  newtype WidgetT m a
    = WidgetT
      { runWidgetT     :: ReaderT Env m a
      }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

  instance (MonadAction m) => MonadAction (WidgetT m)


  -- |
  -- WidgetT environment.
  --
  data Env
    = Env
      { args           :: [(Text, Text)]
      , prefix         :: Text
      }


  -- |
  -- Obtain all widget arguments as pieces of text.
  --
  getArguments :: (Monad m) => WidgetT m [(Text, Text)]
  getArguments = WidgetT do
    Env{args} <- ask
    return args


  -- |
  -- Obtain a specific widget argument and parse it on the fly.
  -- Parsing failure maps to 'Nothing'.
  --
  getArgumentMaybe :: (Monad m, Param a) => Text -> WidgetT m (Maybe a)
  getArgumentMaybe n = do
    value <- lookup n <$> getArguments
    return $ fromParam =<< value


  -- |
  -- Similar to 'getArgumentMaybe', but return either the parsed parameter
  -- or the specified default value.
  --
  getArgumentDefault :: (Monad m, Param a) => Text -> a -> WidgetT m a
  getArgumentDefault n v = fromMaybe v <$> getArgumentMaybe n


  -- |
  -- Obtain a group of request query string parameters with the same name
  -- and parse them on the fly to the target type.
  --
  getArgumentList :: (Monad m, Param a) => Text -> WidgetT m [a]
  getArgumentList n = mapMaybe (fromParam . snd)
                   <$> filter ((n ==) . fst)
                   <$> getArguments


  -- |
  -- Class to render widgets using various HTML/CSS frameworks.
  --
  -- Example:
  --
  -- @
  -- -- For Bootstrap 5
  -- data B5 = B5
  --
  -- data Green a = Green a
  --
  -- instance (Monad m) => 'Render' B5 m (Green Text) where
  --   'render' (Green text) = do
  --     span_ [style_ \"color: green\"] do
  --       toHtml text
  -- @
  --
  class (Monad m) => Render t m a where
    -- |
    -- Render widget as HTML.
    --
    render :: a -> HtmlT m ()


-- vim:set ft=haskell sw=2 ts=2 et:
