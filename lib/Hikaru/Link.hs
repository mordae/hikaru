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
  ( buildLink
  , updateQuery
  , rhref
  , qhref
  , isActiveRoute
  )
where
  import Praha hiding (curry)

  import Hikaru.Action
  import Hikaru.HTML
  import Hikaru.Route
  import Hikaru.Types

  import Data.Binary.Builder
  import Data.HVect
  import Data.List (map, filter)
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
  buildLink :: (HasRep ts, AllHave Param ts)
            => Route ts a
            -> HVectElim ts ([(Text, Text)] -> Text)
  buildLink route = curry (buildLink' route)


  buildLink' :: (AllHave Param ts)
             => Route ts a -> HVect ts -> [(Text, Text)] -> Text
  buildLink' route xs qs = cs (toLazyByteString (path <> toQuery qs))
    where
      path = case routeLinkHVect route xs of
               [] -> "/"
               ps -> encodePathSegments ps


  toQuery :: [(Text, Text)] -> Builder
  toQuery qs = renderQueryBuilder True qs'
    where qs' = flip map qs \(n, v) -> (cs n, Just (cs v))


  -- |
  -- Uses 'rhref' to set HTML 'attr' @href@.
  --
  -- Example:
  --
  -- @
  -- qs <- updateQuery [\"conv\" .= \"upper\"]
  -- 'tag' \"a\" \"\" do
  --   rhref getEchoR \"Hi\" qs
  --   text \"upper\"
  -- @
  --
  rhref :: forall ts m a. (AllHave Param ts, HasRep ts, MonadIO m)
        => Route ts a -> HVectElim ts ([(Text, Text)] -> HtmlT m ())
  rhref route = curry ((rhref' :: Route ts a -> HVect ts -> [(Text, Text)] -> HtmlT m ()) route)


  rhref' :: (MonadIO m, AllHave Param ts)
         => Route ts a -> HVect ts -> [(Text, Text)] -> HtmlT m ()
  rhref' route xs qs = attr [ "href" .= (buildLink' route xs qs) ]


  -- |
  -- Used to set href from a textual URL and a query string.
  --
  -- Can be combined with 'updateQuery' like this:
  --
  -- @
  -- 'tag' \"a\" \"\" do
  --   qhref \"\" [ \"lang\" .= \"en\" ]
  --   text \"English\"
  -- @
  --
  qhref :: (MonadIO m) => Text -> [(Text, Text)] -> HtmlT m ()
  qhref url qs = attr [ "href" .= (url <> cs (toLazyByteString (toQuery qs))) ]


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
