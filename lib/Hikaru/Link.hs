{-|
Module      :  Hikaru.Link
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides various ways to build local links.
-}

module Hikaru.Link
  ( makeLink
  , deriveLink

  -- ** Lucid Integration
  , lhref_
  , phref_
  , qhref_
  )
where
  import BasePrelude

  import Data.Binary.Builder
  import Data.ByteString (ByteString)
  import Data.String.Conversions
  import Data.Text (Text)
  import Lucid
  import Network.HTTP.Types.URI
  import Hikaru.Action


  -- |
  -- Combine path segments and parameters to create an internal Link.
  --
  -- Examples:
  --
  -- >>> makeLink ["api", "echo"] []
  -- "/api/echo"
  -- >>> makeLink ["char", ""] [("name", "haruhi")]
  -- "/char/?name=haruhi"
  --
  makeLink :: [Text] -> [(Text, Text)] -> Text
  makeLink ps qs = cs $ toLazyByteString $ encodePath ps $ csQueryTuple qs


  -- |
  -- Create a link with just the query string by updating the
  -- parameters sent by the client.
  --
  -- All keys that appear in the new parameter list are first deleted
  -- from the current parameter list, then the new list is appended to
  -- the current one.
  --
  -- Useful to create dynamic pages with multiple independent widgets.
  --
  deriveLink :: (MonadAction m) => [(Text, Text)] -> m Text
  deriveLink ps = do
    ops <- getParams
    return $ makeLink [] $ update ops ps


  csQueryTuple :: [(Text, Text)] -> [(ByteString, Maybe ByteString)]
  csQueryTuple = map \(n, v) -> (cs n, Just (cs v))


  update :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)]
  update old new = deleteNewKeys old <> new
    where
      deleteNewKeys = filter \(n, _) -> n `notElem` newKeys
      newKeys = map fst new


  -- Lucid Integration -------------------------------------------------------


  -- |
  -- Create a @href@ attribute using 'makeLink'.
  --
  lhref_ :: [Text] -> [(Text, Text)] -> Attribute
  lhref_ ps qs = href_ (makeLink ps qs)


  -- |
  -- Same as 'lhref_', but without any query parameters.
  --
  phref_ :: [Text] -> Attribute
  phref_ ps = href_ (makeLink ps [])


  -- |
  -- Same as 'lhref_', but without any path components.
  --
  qhref_ :: [(Text, Text)] -> Attribute
  qhref_ qs = href_ (makeLink [] qs)


-- vim:set ft=haskell sw=2 ts=2 et:
