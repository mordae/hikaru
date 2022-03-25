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
  , localize'
  , localizeText
  , localizeText'
  , selectLanguage
  )
where
  import Praha

  import Hikaru.Action
  import Hikaru.HTML
  import Hikaru.Media

  import Blaze.ByteString.Builder (toByteString)

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
    -- Localize the message using given locale.
    --
    localize :: (MonadIO m) => Locale -> l -> HtmlT m ()
    localize _lang msg = text (tshow msg)
    {-# INLINE localize #-}


  -- |
  -- Instance to make 'Text' usable for interoperability and
  -- gradual localization.
  --
  instance Localizable Text where
    localize _lang msg = text msg
    {-# INLINE localize #-}


  -- |
  -- Simplify localization of optional messages.
  --
  instance (Localizable l) => Localizable (Maybe l) where
    localize lang (Just msg) = localize lang msg
    localize ____ Nothing    = pass


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguage' of the current action. Uses 'localize'.
  --
  localize' :: (MonadAction m, Localizable l) => l -> HtmlT m ()
  localize' msg = do
    lang <- getLanguage
    localize lang msg
  {-# INLINE localize' #-}


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguage' of the current action. Uses 'localizeText'.
  --
  localizeText' :: (MonadAction m, Localizable l) => l -> m Text
  localizeText' msg = do
    lang <- getLanguage
    localizeText lang msg

  {-# INLINE localizeText' #-}


  -- |
  -- Localize the message using given locale and convert it
  -- to 'Text' using 'plainHtmlT'.
  --
  localizeText :: (MonadIO m, Localizable l) => Text -> l -> m Text
  localizeText lang msg = do
    res <- plainHtmlT $ localize lang msg
    return $ cs $ toByteString res

  {-# INLINE localizeText #-}


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
    acceptable <- bestLanguage <$> getAcceptLanguage

    case preferred of
      Just lang -> setCookie cookieName lang
      Nothing   -> pass

    setLanguage $ fromMaybe fallback $ preferred <|> previous <|> listToMaybe acceptable


  bestLanguage :: [Media] -> [Text]
  bestLanguage = map (.mainType)
               . reverse
               . sortOn (.quality)
               . filter ((> 0) . (.quality))


-- vim:set ft=haskell sw=2 ts=2 et:
