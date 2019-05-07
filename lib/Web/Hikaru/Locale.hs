{-|
Module      :  Web.Hikaru.Locale
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides support for website localization.

Example:

@
data SampleMessages
  = MsgSuccess
  | MsgFailure

instance 'ToHtml' SampleMessages where
  toHtmlRaw = toHtml
  toHtml MsgSuccess = \"Success\"
  toHtml MsgFailure = \"Failure\"

instance Localized SampleMessages where
  localize \"cs\" MsgSuccess = Just \"Úspěch\"
  localize \"cs\" MsgFailure = Just \"Selhání\"
  localize _locale _msg    = Nothing

getSampleR :: Bool -> Action ()
getSampleR flag = do
  'sendHTML' $ do
    if flag
       then lc_ MsgSuccess
       else lc_ MsgFailure
@
-}

module Web.Hikaru.Locale
  ( Locale
  , Localized(..)
  , lc_
  )
where
  import BasePrelude

  import Data.Text (Text)
  import Lucid
  import Web.Hikaru.Action


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
    --
    -- Return 'Nothing' for any unsupported locale and provide a 'toHtml'
    -- method to be used when no good locale is available.
    --
    localize :: (Monad m) => Locale -> a -> Maybe (HtmlT m ())
    localize _lc _msg = Nothing


  -- |
  -- Localize given message to the language indicated by the 'getLanguages'
  -- function executed in the context of the current action.
  --
  lc_ :: (MonadAction m, Localized a) => a -> HtmlT m ()
  lc_ msg = do
    langs <- getLanguages

    case mapMaybe (flip localize msg) langs of
      []  -> toHtml msg
      x:_ -> x


-- vim:set ft=haskell sw=2 ts=2 et:
