-- |
-- Module      :  Hikaru.HTML
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides a simple HTML builder monad with focus on readability.
-- It utilizes the venerable Blaze Builder.
--
-- Example:
--
-- @
-- builder <- 'fromHtmlT' do
--   'tag' \"a\" \"link-pretty px-2 py-1\" do
--     'attr' [ \"href\" '.=' \"/help\" ]
--     'tag' \"i\" "bi bi-question-diamond" pass
--     'text' \" Need help?\"
-- @
--

module Hikaru.HTML
  (
    -- * Types
    HtmlT
  , Html(..)

    -- * Rendering
  , runHtmlT
  , fromHtmlT
  , fromHtml
  , plainHtmlT
  , plainHtml

    -- * Authoring
  , tag
  , tag'
  , attr
  , text
  , html
  , doctype
  )
where
  import Praha
  import Praha.Logger

  import Blaze.ByteString.Builder
  import Blaze.ByteString.Builder.Html.Utf8
  import UnliftIO.IORef

  import Data.List (reverse, filter, sortOn)


  -- |
  -- Fragment of HTML code.
  --
  data Html
    = HtmlTag Text [Html]
      -- ^ Tag with a name and a body.
    | HtmlEmptyTag Text [Html]
      -- ^ Unclosed tag with just attributes.
    | HtmlRaw Text
      -- ^ Raw chunk of HTML to be rendered directly.
    | HtmlEscaped Text
      -- ^ Chunk of text to be escaped upon rendering.
    | HtmlAttribute Text Text
      -- ^ Attribute on the parent tag.
    deriving (Show, Eq)


  -- |
  -- HTML building context.
  --
  -- Supports 'tag', 'attr' and other authoring functions.
  --
  newtype HtmlT m a
    = HtmlT
      { stack          :: ReaderT (IORef [Html]) m a
      }
    deriving ( Functor
             , Applicative
             , Alternative
             , Contravariant
             , Monad
             , MonadFail
             , MonadFix
             , MonadIO
             , MonadLogger
             , MonadPlus
             , MonadTrans
             , MonadUnliftIO
             )


  emitHtml :: (MonadIO m) => Html -> ReaderT (IORef [Html]) m ()
  emitHtml body = do
    ioref <- ask
    modifyIORef' ioref (body :)

  {-# INLINE emitHtml #-}


  -- |
  -- Build the 'Html' tree.
  --
  runHtmlT :: (MonadIO m) => HtmlT m a -> m (a, [Html])
  runHtmlT HtmlT{stack} = do
    ioref <- newIORef []
    res <- runReaderT stack ioref
    children <- readIORef ioref
    return (res, reverse children)


  -- |
  -- Add a tag with opening and closing mark, possibly holding children.
  --
  tag :: (MonadIO m)
      => Text                -- ^ Tag name
      -> Text                -- ^ Classes
      -> HtmlT m a           -- ^ Children
      -> HtmlT m a
  tag name classes HtmlT{stack} = HtmlT do
    ioref <- newIORef []
    res <- local (const ioref) stack

    children <- readIORef ioref

    if classes /= ""
       then emitHtml $ HtmlTag name (HtmlAttribute "class" classes : reverse children)
       else emitHtml $ HtmlTag name (reverse children)

    return res

  {-# INLINE tag #-}


  -- |
  -- Add a tag without the closing mark.
  --
  tag' :: (MonadIO m)
       => Text               -- ^ Tag name
       -> Text               -- ^ Classes
       -> [(Text, Text)]     -- ^ Attributes
       -> HtmlT m ()
  tag' name classes attrs = HtmlT do
    let children = fmap (uncurry HtmlAttribute) attrs

    if classes /= ""
       then emitHtml $ HtmlEmptyTag name (HtmlAttribute "class" classes : children)
       else emitHtml $ HtmlEmptyTag name children

  {-# INLINE tag' #-}


  -- |
  -- Add some attributes to the parent 'tag'.
  --
  -- When an attribute with the same name is added twice, the values
  -- are joined together, delimited by spaces.
  --
  attr :: (MonadIO m) => [(Text, Text)] -> HtmlT m ()
  attr attrs = HtmlT do
    forM_ attrs \(name, val) -> do
      emitHtml $ HtmlAttribute name val

  {-# INLINE attr #-}


  -- |
  -- Add an escaped text.
  --
  text :: (MonadIO m) => Text -> HtmlT m ()
  text body = HtmlT $ emitHtml $ HtmlEscaped body
  {-# INLINE text #-}


  -- |
  -- Add a raw chunk of HTML code.
  --
  html :: (MonadIO m) => Text -> HtmlT m ()
  html body = HtmlT $ emitHtml $ HtmlRaw body
  {-# INLINE html #-}


  -- |
  -- Add the HTML5 doctype of @<!doctype html>@ followed by a newline.
  --
  doctype :: (MonadIO m) => HtmlT m ()
  doctype = html "<!doctype html>\n"
  {-# INLINE doctype #-}


  -- |
  -- Convert 'HtmlT' to a 'Builder'.
  --
  fromHtmlT :: (MonadIO m) => HtmlT m a -> m Builder
  fromHtmlT body = do
    (_res, frags) <- runHtmlT body
    return $ mconcat $ fmap fromHtml frags


  -- |
  -- Extract just unescaped 'HtmlEscaped' data, effectively turning
  -- the HTML fragment into plain text. Ignores 'HtmlRaw' fragments.
  --
  plainHtmlT :: (MonadIO m) => HtmlT m a -> m Builder
  plainHtmlT body = do
    (_res, frags) <- runHtmlT body
    return $ mconcat $ fmap plainHtml frags


  -- |
  -- Extract just unescaped 'HtmlEscaped' data.
  --
  plainHtml :: Html -> Builder
  plainHtml (HtmlEscaped esc) = fromText esc
  plainHtml _other = mempty


  -- |
  -- Convert 'Html' tree to a 'Builder'.
  --
  fromHtml :: Html -> Builder
  fromHtml (HtmlRaw raw) =
    fromText raw

  fromHtml (HtmlEscaped esc) =
    fromHtmlEscapedText esc

  fromHtml (HtmlAttribute name "") =
    mconcat [ fromText " "
            , fromText name
            ]

  fromHtml (HtmlAttribute name val) =
    mconcat [ fromText " "
            , fromText name
            , fromText "=\""
            , fromHtmlEscapedText val
            , fromText "\""
            ]

  fromHtml (HtmlTag name children) =
    mconcat [ fromText "<"
            , fromText name
            , mconcat $ fmap fromHtml $ mergeAttr $ sortOn htmlSort $ filter isAttr children
            , fromText ">"
            , mconcat $ fmap fromHtml $ filter (not . isAttr) children
            , fromText "</"
            , fromText name
            , fromText ">"
            ]

  fromHtml (HtmlEmptyTag name children) =
    mconcat [ fromText "<"
            , fromText name
            , mconcat $ fmap fromHtml $ mergeAttr $ sortOn htmlSort $ filter isAttr children
            , fromText ">"
            ]


  isAttr :: Html -> Bool
  isAttr (HtmlAttribute _ _) = True
  isAttr _ = False


  htmlSort :: Html -> Maybe Text
  htmlSort (HtmlAttribute name _) = Just name
  htmlSort _otherwise             = Nothing


  mergeAttr :: [Html] -> [Html]
  mergeAttr = foldr merge []
    where
      merge head@(HtmlAttribute name0 val0)
            tail@(HtmlAttribute name1 val1 : rest) =
        if name0 == name1
           then HtmlAttribute name0 (val0 <> " " <> val1) : rest
           else head : tail

      merge head tail = head : tail


-- vim:set ft=haskell sw=2 ts=2 et:
