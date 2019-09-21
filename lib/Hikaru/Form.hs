{-|
Module      :  Hikaru.Form
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides tools to simplify form building and parsing.
-}

module Hikaru.Form
  ( Form
  , FormHandler
  , handleForm

  -- ** Inputs
  , formInput
  , formMultiple
  , formOptions
  , selectedOption
  , selectedOptions
  )
where
  import BasePrelude

  import Data.ByteString.Builder (toLazyByteString)
  import Data.Text (Text)
  import Lucid
  import Hikaru.Action
  import Hikaru.Types


  -- |
  -- HTML form with a potentially parsed object.
  --
  type Form m a = HtmlT m (Maybe a)


  -- |
  -- Function that takes either a form to present to the user or
  -- a fully parsed object to somehow handle.
  --
  type FormHandler m a = Either (HtmlT m ()) a -> m ()


  -- |
  -- Simplifies handling form submissions.
  --
  -- Example:
  --
  -- @
  -- handleForm someForm \\case
  --   Left incompleteForm -> do
  --     sendHTML do
  --       genericPage_ \"Nice Form\" $ do
  --         incompleteForm
  --
  --   Right entry -> do
  --     processEntry entry
  --     redirect \"\/entries\/\"
  -- @
  --
  handleForm :: (Monad m) => Form m a -> FormHandler m a -> m ()
  handleForm form decide = do
    (mb, mx) <- runHtmlT form

    case mx of
      -- A little hack to get both the HtmlT and its value and shield the
      -- caller from 'Builder' and 'toHtmlRaw'.
      Nothing -> decide $ Left (toHtmlRaw $ toLazyByteString $ mb mempty)
      Just x  -> decide $ Right x


  -- |
  -- Read a single form @\<input\>@ field as both the original text and the
  -- converted value.
  --
  -- Return 'Nothing' if the request was submitted using one of the HTTP
  -- methods that do not support request bodies (such as @GET@ or @HEAD@).
  --
  formInput :: (MonadAction m, FromParam a) => Text -> m (Text, Maybe a)
  formInput name = do
    caseMethod ("", Nothing) do
      text  <- fromMaybe "" <$> getFieldMaybe name
      return (text, fromParam text)


  -- |
  -- Read multiple form @\<input\>@ fields as both the original texts and the
  -- converted values.
  --
  -- Return empty list if the request was submitted using one of the HTTP
  -- methods that do not support request bodies (such as @GET@ or @HEAD@).
  --
  formMultiple :: (MonadAction m, FromParam a) => Text -> m [(Text, a)]
  formMultiple name = do
    caseMethod [] do
      getFieldList name
      <&> mapMaybe \x -> case fromParam x of
                           Nothing -> Nothing
                           Just v  -> Just (x, v)


  -- |
  -- Read selected @\<option\>@ fields and return a list of all available
  -- fields along with their selection status.
  --
  formOptions :: (MonadAction m, FromParam a, ToParam a, Eq a)
              => Text -> [a] -> m [(Text, a, Bool)]
  formOptions name options = do
    found <- caseMethod [] (getFieldList name)
    return $ flip map options \v -> (toParam v, v, v `elem` found)


  -- |
  -- Return the first selected value in the option list returned by
  -- 'formOptions'.
  --
  selectedOption :: [(Text, a, Bool)] -> Maybe a
  selectedOption = listToMaybe . selectedOptions


  -- |
  -- Return all selected values in the option list returned by
  -- 'formOptions'.
  --
  selectedOptions :: [(Text, a, Bool)] -> [a]
  selectedOptions = mapMaybe (\(_, v, s) -> if s then Just v else Nothing)


  -- Helper Functions --------------------------------------------------------


  -- |
  -- Run action only for methods that come with bodies.
  -- Otherwise return the default value.
  --
  caseMethod :: (MonadAction m) => a -> m a -> m a
  caseMethod dfl action = do
    method <- getMethod

    case method of
      "GET"     -> return dfl
      "HEAD"    -> return dfl
      "CONNECT" -> return dfl
      "OPTIONS" -> return dfl
      "TRACE"   -> return dfl
      _else     -> action



-- vim:set ft=haskell sw=2 ts=2 et:
