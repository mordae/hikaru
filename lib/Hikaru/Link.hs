-- |
-- Module      :  Hikaru.Link
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides various ways to build local links and provide
-- feedback based on the request path.
--

module Hikaru.Link
  ( rhref
  , rhref_
  , updateQuery
  , isActiveRoute
  )
where
  import Praha hiding (curry)

  import Hikaru.Action
  import Hikaru.Route
  import Hikaru.Types

  import Data.Binary.Builder
  import Data.HVect
  import Data.List (map, filter)
  import Lucid
  import Network.HTTP.Types.URI


  -- |
  -- Construct a link with a query string to the given route,
  -- using given values for captured segments.
  --
  -- Example:
  --
  -- >>> rhref getEchoR "Hi" [("q", "42")]
  -- "/echo/Hi?q=42"
  --
  rhref :: (HasRep ts, AllHave Param ts)
        => Route ts a
        -> HVectElim ts ([(Text, Text)] -> Text)
  rhref route = curry (hvRhref route)


  hvRhref :: (AllHave Param ts)
          => Route ts a -> HVect ts -> [(Text, Text)] -> Text
  hvRhref route xs qs = buildLink route xs qs


  -- |
  -- Similar to 'rhref', but inteded to be used with Lucid.
  --
  -- Example:
  --
  -- @
  -- qs <- updateQuery [("conv", "upper")]
  -- 'a_' ['rhref_' getEchoR "Hi" qs] ...
  -- @
  --
  rhref_ :: (HasRep ts, AllHave Param ts)
         => Route ts a
         -> HVectElim ts ([(Text, Text)] -> Attribute)
  rhref_ route = curry (hvRhref_ route)


  hvRhref_ :: (AllHave Param ts)
           => Route ts a -> HVect ts -> [(Text, Text)] -> Attribute
  hvRhref_ route xs qs = href_ (buildLink route xs qs)


  buildLink :: (AllHave Param ts)
            => Route ts a -> HVect ts -> [(Text, Text)] -> Text
  buildLink route xs qs = cs (toLazyByteString (path <> query))
    where
      path = case routeLinkHVect route xs of
               [] -> "/"
               ps -> encodePathSegments ps

      query = renderQueryBuilder True $
                flip map qs \(n, v) -> (cs n, Just (cs v))


  -- |
  -- Update query string from the original request with supplied values.
  --
  -- First, all keys in the supplied query are removed from the original,
  -- then the supplied query gets appended to the original.
  --
  updateQuery :: (MonadAction m) => [(Text, Text)] -> m [(Text, Text)]
  updateQuery query' = do query <- filter (not. updated) <$> getParams
                          return (query <> query')
    where
      updated (key, _) = key `elem` map fst query'


  -- Path Feedback -----------------------------------------------------------


  -- |
  -- Determine whether the supplied route is the one user has requested.
  --
  isActiveRoute :: (MonadAction m) => Route ts a -> m Bool
  isActiveRoute route = isJust <$> flip routeApply route <$> getPathInfo


-- vim:set ft=haskell sw=2 ts=2 et:
