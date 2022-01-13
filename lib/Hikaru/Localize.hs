-- |
-- Module      :  Hikaru.Localize
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides support for website localization.
--
-- First, you need to create a message catalog:
--
-- @
-- -- All messages we want to localize.
-- data SampleMessages
--   = MsgSuccess
--   | MsgFailure
--   deriving (Show)
--
-- -- Default HTML rendering of the messages.
-- instance 'ToHtml' SampleMessages where
--   'toHtmlRaw' = 'toHtml'
--
-- -- Language-specific rendering of those messages.
-- instance Localizable SampleMessages where
--   -- English variants
--   'localize' \"en\" MsgSuccess = 'Just' \"Success\"
--   'localize' \"en\" MsgFailure = 'Just' \"Failure\"
--
--   -- Czech variants
--   'localize' \"cs\" MsgSuccess = 'Just' \"Úspěch\"
--   'localize' \"cs\" MsgFailure = 'Just' \"Selhání\"
--
--   -- Otherwise try the next locale
--   'localize' _locale _msg = 'Nothing'
-- @
--
-- Next, create a preferred language list for every action:
--
-- @
-- 'Hikaru.Dispatch.dispatch' runAction $ do
--   'Hikaru.Dispatch.wrapAction' ('selectLanguages' \"lang\" \"lang\" >>) $ do
--     'Hikaru.Dispatch.route' ...
-- @
--
-- Finally, you can use your catalog when rendering pages:
--
-- @
-- getSampleR :: 'Bool' -> Action ()
-- getSampleR flag = do
--   'sendHTML' $ do
--     if flag
--        then lc_ MsgSuccess
--        else lc_ MsgFailure
-- @
--

module Hikaru.Localize
  ( Locale
  , Localizable(..)
  , lc
  , lc_
  , selectLanguages
  )
where
  import Praha

  import Data.List (filter, map, nub)
  import Hikaru.Action
  import Hikaru.Media
  import Lucid


  -- |
  -- Name of the target locale.
  --
  -- Usually an ISO 639-1 Alpha-2 code such as @en@ or @cs@.
  --
  type Locale = Text


  -- |
  -- Any message that can be rendered localized.
  --
  class (Show l) => Localizable l where
    -- |
    -- Try to localize the message using given locale.
    -- Return 'Nothing' if the locale is not supported.
    --
    localize :: Locale -> l -> Maybe Text
    localize _lang _msg = Nothing
    {-# INLINE localize #-}

    -- |
    -- Same as 'localize', but for HTML.
    -- Defaults to using the plain 'localize'.
    --
    localizeHtml :: (Monad m) => Locale -> l -> Maybe (HtmlT m ())
    localizeHtml lang = fmap toHtml . localize lang
    {-# INLINE localizeHtml #-}


  -- |
  -- Instance to make 'Text' usable for interoperability and
  -- gradual localization.
  --
  instance Localizable Text where
    localize _lc = Just
    {-# INLINE localize #-}


  -- |
  -- Instance to make 'Maybe' values simpler to render localized.
  --
  instance Localizable l => Localizable (Maybe l) where
    localize lang (Just msg) = localize lang msg
    localize _ Nothing       = Just ""
    {-# INLINE localize #-}


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguages' of the current action. Uses 'localize' internaly.
  --
  lc :: (MonadAction m, Localizable l) => l -> m Text
  lc msg = do
    langs <- getLanguages

    case mapMaybe (flip localize msg) langs of
      []  -> return $ tshow msg
      x:_ -> return x


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguages' of the current action.
  --
  lc_ :: (MonadAction m, Localizable l) => l -> HtmlT m ()
  lc_ msg = do
    langs <- getLanguages

    case mapMaybe (flip localizeHtml msg) langs of
      []  -> toHtml $ tshow msg
      x:_ -> x


  -- |
  -- Determine preferred language order using a parameter, cookie
  -- and finally the @Accept-Language@ header items.
  --
  -- If the parameter is present, also set the cookie so that the
  -- primary language preference persists across multiple page loads.
  --
  selectLanguages :: (MonadAction m) => Text -> Text -> m ()
  selectLanguages paramName cookieName = do
    preferred <- getParamMaybe paramName
    previous  <- getCookieMaybe cookieName
    acceptable <- getAcceptLanguage
                  <&> filter ((> 0) . (.quality))
                  <&> map (.mainType)

    case preferred of
      Nothing   -> return ()
      Just lang -> setCookie cookieName (cs lang)

    setLanguages $ nub $ maybeToList preferred
                         <> maybeToList previous
                         <> acceptable


-- vim:set ft=haskell sw=2 ts=2 et:
