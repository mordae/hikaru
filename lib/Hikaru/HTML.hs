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

    -- * Rendering
  , runHtmlT
  , fromHtmlT
  , plainHtmlT

    -- * Authoring
  , tag
  , tag'
  , attr
  , text
  , html
  , doctype
  )
where
  import Praha hiding (toList)

  import Data.ByteString.Builder
  import Data.Map (insert)
  import Data.Map.Merge.Strict
  import Data.Text.Encoding
  import UnliftIO.IORef

  import GHC.Exts (Item, toList)

  import Data.ByteString.Builder.Prim (BoundedPrim, (>$<), (>*<))

  import qualified Data.ByteString.Builder.Prim as P


  -- |
  -- HTML building context.
  --
  -- Supports 'tag', 'attr' and other authoring functions.
  --
  newtype HtmlT m a
    = HtmlT
      { stack          :: ReaderT Env m a
      }
    deriving ( Functor
             , Applicative
             , Alternative
             , Contravariant
             , Monad
             , MonadFail
             , MonadFix
             , MonadIO
             , MonadPlus
             , MonadTrans
             , MonadUnliftIO
             )


  data Env
    = Env
      { envAttrs       :: IORef Attributes
      , envInner       :: IORef Builder
      , envRich        :: Bool
      }


  data Attributes
    = Attributes
      { attributes     :: Map Text Text
      }

  instance IsList Attributes where
    type Item Attributes = (Text, Text)
    fromList list = Attributes (fromList list)
    toList Attributes{attributes} = toList attributes

  instance Semigroup Attributes where
    Attributes a <> Attributes b =
      let cat _key new old = old <> " " <> new
       in Attributes (merge preserveMissing preserveMissing (zipWithMatched cat) a b)

  instance Monoid Attributes where
    mempty = Attributes mempty


  -- |
  -- Build the 'Html' tree.
  --
  runHtmlT :: (MonadIO m) => HtmlT m a -> m (a, Builder)
  runHtmlT HtmlT{stack} = do
    envAttrs <- newIORef mempty
    envInner <- newIORef mempty

    res <- runReaderT stack Env{envRich = True, ..}

    builder <- readIORef envInner
    return (res, builder)


  -- |
  -- Extract just unescaped 'text' chunks, effectively turning the
  -- HTML fragment into plain text.
  --
  plainHtmlT :: (MonadIO m) => HtmlT m a -> m Builder
  plainHtmlT HtmlT{stack} = do
    envAttrs <- newIORef mempty
    envInner <- newIORef mempty

    void do
      runReaderT stack Env{envRich = False, ..}

    builder <- readIORef envInner
    return builder


  -- |
  -- Convert 'HtmlT' to a 'Builder'.
  --
  fromHtmlT :: (MonadIO m) => HtmlT m a -> m Builder
  fromHtmlT = fmap snd . runHtmlT


  -- |
  -- Add a tag with opening and closing mark, possibly holding children.
  --
  tag :: (MonadIO m)
      => Text                -- ^ Tag name
      -> Text                -- ^ Classes
      -> HtmlT m a           -- ^ Children
      -> HtmlT m a
  tag name classes HtmlT{stack} = HtmlT do
    Env{envRich} <- ask
    envAttrs <- newIORef mempty
    envInner <- newIORef mempty

    unless (classes == "") do
      modifyIORef envAttrs \(Attributes am) ->
        Attributes (insert "class" classes am)

    res <- lift (runReaderT stack Env{..})

    attrs <- readIORef envAttrs
    inner <- readIORef envInner
    emitTag name attrs inner

    return res

  {-# NOINLINE tag #-}


  -- |
  -- Add a tag without the closing mark.
  --
  tag' :: (MonadIO m)
       => Text               -- ^ Tag name
       -> Text               -- ^ Classes
       -> [(Text, Text)]     -- ^ Attributes
       -> HtmlT m ()
  tag' name classes attrs = HtmlT do
    if classes /= ""
       then emitUnclosed name (fromList $ ("class", classes) : attrs)
       else emitUnclosed name (fromList attrs)

  {-# NOINLINE tag' #-}


  -- |
  -- Add some attributes to the parent 'tag'.
  --
  -- When an attribute with the same name is added twice, the values
  -- are joined together, delimited by spaces.
  --
  attr :: (MonadIO m) => [(Text, Text)] -> HtmlT m ()
  attr attrs = HtmlT do
    Env{envAttrs} <- ask
    modifyIORef envAttrs (<> fromList attrs)

  {-# INLINE attr #-}


  -- |
  -- Add an escaped text.
  --
  text :: (MonadIO m) => Text -> HtmlT m ()
  text = HtmlT . emitEscaped
  {-# INLINE text #-}


  -- |
  -- Add a raw chunk of HTML code.
  --
  html :: (MonadIO m) => Text -> HtmlT m ()
  html = HtmlT . emitRaw
  {-# INLINE html #-}


  -- |
  -- Add the HTML5 doctype of @<!doctype html>@ followed by a newline.
  --
  doctype :: (MonadIO m) => HtmlT m ()
  doctype = HtmlT (emitHtml "<!doctype html>\n")
  {-# INLINE doctype #-}


  emitEscaped :: (MonadIO m) => Text -> ReaderT Env m ()
  emitEscaped t = do
    Env{envRich, envInner} <- ask

    if envRich
       then modifyIORef envInner (<> escaped t)
       else modifyIORef envInner (<> raw t)

  {-# INLINE emitEscaped #-}


  emitHtml :: (MonadIO m) => Builder -> ReaderT Env m ()
  emitHtml builder = do
    Env{envRich, envInner} <- ask

    when envRich do
      modifyIORef envInner (<> builder)

  {-# INLINE emitHtml #-}


  emitRaw :: (MonadIO m) => Text -> ReaderT Env m ()
  emitRaw = emitHtml . raw
  {-# INLINE emitRaw #-}


  emitTag :: (MonadIO m) => Text -> Attributes -> Builder -> ReaderT Env m ()
  emitTag name attrs inner = do
    let attrs' = foldMap buildAttr (toList attrs)
    emitHtml $ "<" <> raw name <> attrs' <> ">" <> inner <> "</" <> raw name <> ">"

  {-# INLINE emitTag #-}


  emitUnclosed :: (MonadIO m) => Text -> Attributes -> ReaderT Env m ()
  emitUnclosed name attrs = do
    emitHtml $ "<" <> raw name <> foldMap buildAttr (toList attrs) <> ">"

  {-# INLINE emitUnclosed #-}


  buildAttr :: (Text, Text) -> Builder
  buildAttr (key, value) = " " <> raw key <> "=\"" <> escaped value <> "\""
  {-# INLINE buildAttr #-}


  raw :: Text -> Builder
  raw = {-# SCC raw #-} encodeUtf8Builder
  {-# INLINE raw #-}


  escaped :: Text -> Builder
  escaped = {-# SCC escaped #-} encodeUtf8BuilderEscaped escape
  {-# INLINE escaped #-}


  escape :: BoundedPrim Word8
  escape =
    P.condB (>= 63) (P.liftFixedToBounded P.word8) $
      P.condB (== 60) (fixed4 (38, (108, (116, 59)))) $                       -- &lt;
        P.condB (== 62) (fixed4 (38, (103, (116, 59)))) $                     -- &gt;
          P.condB (== 38) (fixed5 (38, (97, (109, (112, 59))))) $             -- &amp;
            P.condB (== 34) (fixed6 (38, (113, (117, (111, (116, 59)))))) $   -- &quot;
              P.condB (== 39) (fixed6 (38, (97, (112, (111, (115, 59)))))) $  -- &apos;
                P.liftFixedToBounded P.word8
    where
      fixed4 x = P.liftFixedToBounded $ const x >$<
        P.word8 >*< P.word8 >*< P.word8 >*< P.word8

      fixed5 x = P.liftFixedToBounded $ const x >$<
        P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8

      fixed6 x = P.liftFixedToBounded $ const x >$<
        P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8

  {-# INLINE escape #-}


-- vim:set ft=haskell sw=2 ts=2 et:
