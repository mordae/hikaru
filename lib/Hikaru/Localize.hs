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
  , localizeText'
  , localizeHtml'
  , selectLanguage
  )
where
  import Praha

  import Hikaru.Action
  import Hikaru.HTML
  import Hikaru.Media

  import Data.List (filter, map, sortOn, reverse)


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
    --
    localizeText :: Locale -> l -> Text
    localizeText _lang msg = tshow msg
    {-# INLINE localizeText #-}

    -- |
    -- Same as 'localize', but for HTML.
    -- Defaults to using the plain 'localize'.
    --
    localizeHtml :: (MonadIO m) => Locale -> l -> HtmlT m ()
    localizeHtml lang msg = text (localizeText lang msg)
    {-# INLINE localizeHtml #-}


  -- |
  -- Instance to make 'Text' usable for interoperability and
  -- gradual localization.
  --
  instance Localizable Text where
    localizeText _lc msg = msg
    {-# INLINE localizeText #-}


  -- |
  -- Instance to make 'Maybe' values simpler to render localized.
  --
  instance Localizable l => Localizable (Maybe l) where
    localizeText lang (Just msg) = localizeText lang msg
    localizeText _ Nothing       = ""
    {-# INLINE localizeText #-}


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguage' of the current action. Uses 'localizeText'.
  --
  localizeText' :: (MonadAction m, Localizable l) => l -> m Text
  localizeText' msg = do
    lang <- getLanguage
    return $ localizeText lang msg


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguage' of the current action. Uses 'localizeHtml'.
  --
  localizeHtml' :: (MonadAction m, Localizable l) => l -> HtmlT m ()
  localizeHtml' msg = do
    lang <- getLanguage
    localizeHtml lang msg


  -- |
  -- Determine preferred language using a parameter, cookie
  -- and finally the @Accept-Language@ header items.
  --
  -- If the parameter is present, also set the cookie so that the
  -- language preference persists across multiple page loads.
  --
  selectLanguage :: (MonadAction m) => Text -> Text -> Text -> m ()
  selectLanguage paramName cookieName fallback = do
    preferred <- getParamMaybe paramName
    previous  <- getCookieMaybe cookieName
    acceptable <- getAcceptLanguage
                  <&> filter ((> 0) . (.quality))
                  <&> sortOn (.quality)
                  <&> reverse
                  <&> map (.mainType)

    case preferred of
      Nothing   -> return ()
      Just lang -> setCookie cookieName lang

    setLanguage $ fromMaybe fallback $ preferred <|> previous <|> listToMaybe acceptable


-- vim:set ft=haskell sw=2 ts=2 et:
