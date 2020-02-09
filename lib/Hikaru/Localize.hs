{-|
Module      :  Hikaru.Localize
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides support for website localization.

First, you need to create a message catalog:

@
-- All messages we want to localize.
data SampleMessages
  = MsgSuccess
  | MsgFailure

-- Default HTML rendering of the messages.
instance 'ToHtml' SampleMessages where
  'toHtmlRaw' = 'toHtml'
  'toHtml' MsgSuccess = \"Success\"
  'toHtml' MsgFailure = \"Failure\"

-- Language-specific rendering of those messages.
instance Localized SampleMessages where
  -- Czech variants
  localize \"cs\" MsgSuccess = 'Just' \"Úspěch\"
  localize \"cs\" MsgFailure = 'Just' \"Selhání\"

  -- English is the default
  localize \"en\" msg        = 'Just' ('toHtml' msg)

  -- Otherwise try the next locale
  localize _locale _msg    = 'Nothing'
@

Next, create a preferred language list for every action:

@
'Hikaru.Dispatch.dispatch' runAction $ do
  'Hikaru.Dispatch.wrapActions' ('selectLanguages' \"lang\" \"lang\" >>)

  'Hikaru.Dispatch.route' ...
@

Finally, you can use your catalog when rendering pages:

@
getSampleR :: 'Bool' -> Action ()
getSampleR flag = do
  'sendHTML' $ do
    if flag
       then lc_ MsgSuccess
       else lc_ MsgFailure
@
-}

module Hikaru.Localize
  ( Locale
  , Localized(..)
  , lc_
  , selectLanguages
  )
where
  import BasePrelude

  import Data.String.Conversions
  import Data.Text (Text)
  import Lucid
  import Hikaru.Action
  import Hikaru.Media


  -- |
  -- Name of the target locale.
  --
  -- Usually an ISO 639-1 Alpha-2 code such as @en@ or @cs@.
  --
  type Locale = Text


  -- |
  -- Any message that can be rendered localized.
  --
  class (ToHtml a) => Localized a where
    -- |
    -- Try to localize the message using given locale.
    -- Return 'Nothing' if the locale is not supported.
    --
    localize :: (Monad m) => Locale -> a -> Maybe (HtmlT m ())
    localize _lc = const Nothing
    {-# INLINE localize #-}


  -- |
  -- Instance to make 'Text' usable for interoperability and
  -- gradual localization.
  --
  instance Localized Text where
    localize _lc = Just . toHtml
    {-# INLINE localize #-}


  -- |
  -- Localize given message to the language indicated by the
  -- 'getLanguages' of the current action.
  --
  lc_ :: (MonadAction m, Localized a) => a -> HtmlT m ()
  lc_ msg = do
    langs <- getLanguages

    case mapMaybe (flip localize msg) langs of
      []  -> toHtml msg
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
                  <&> filter ((> 0) . mediaQuality)
                  <&> map mediaMainType

    case preferred of
      Nothing   -> return ()
      Just lang -> setCookie cookieName (cs lang)

    setLanguages $ nub $ maybeToList preferred
                         <> maybeToList previous
                         <> acceptable


-- vim:set ft=haskell sw=2 ts=2 et:
