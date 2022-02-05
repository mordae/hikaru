-- |
-- Module      :  Hikaru.Dispatch
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides means for dispatching on routes.
--
-- For example:
--
-- @
-- demo :: 'Application'
-- demo = 'dispatch' id do
--   'middleware' logStdout
--   'handler' 404 notFound
--   'route' getHelloR
--   'route' getByeR
-- @
--

module Hikaru.Dispatch
  ( Dispatch
  , dispatch
  , route
  , middleware
  , handler
  )
where
  import Praha hiding (curry)

  import Hikaru.Action (abortMiddleware)
  import Hikaru.Route

  import Data.List (sortOn, reverse, lookup)
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import Network.Wai


  -- |
  -- Since routes do not share a common type (due to their captured parameters
  -- being part of their type), we cannot just pass them in a list. So for
  -- convenience, there is a dispatcher monad that also helps with middleware
  -- and error handler registration.
  --
  newtype Dispatch h a
    = Dispatch
      { runDispatch    :: State (Env h) a
      }
    deriving (Functor, Applicative, Monad)


  data Env h
    = Env
      { routes         :: [Request -> Maybe (h, Score)]
      , mwstack        :: Middleware
      , handlers       :: [(Int, Response -> h)]
      }


  initEnv :: Env h
  initEnv = Env { routes   = []
                , mwstack  = id
                , handlers = []
                }


  -- |
  -- Try to dispatch on a route.
  --
  -- All routes are tried and scored for every request,
  -- preferring the earlier one in case of a tie.
  --
  route :: Route (ts :: [Type]) h -> Dispatch h ()
  route r = Dispatch do
    let r' req = case routeApply (pathInfo req) r of
                   Just h  -> Just (h, routeScore req r)
                   Nothing -> Nothing

    -- Routes must be prepended since we will later sort them and
    -- reverse the order, making the earlier matching routes come
    -- out first.
    modify \e@Env{..} -> e { routes = r' : routes }


  -- |
  -- Register middleware.
  --
  -- Middleware gets applied in the reverse order of its appearence,
  -- but always after the error handler.
  --
  middleware :: Middleware -> Dispatch h ()
  middleware mw = Dispatch do
    modify \e@Env{..} -> e { mwstack = mwstack . mw }


  -- |
  -- Register error handler.
  --
  -- It gets called when some of the routes respond with given status code.
  -- Middleware gets applied after the handler, not before.
  --
  handler :: Int -> (Response -> h) -> Dispatch h ()
  handler code fn = Dispatch do
    modify \e@Env{..} -> e { handlers = (code, fn) : handlers }


  -- |
  -- Perform the dispatching.
  --
  -- Needs a runner function that converts whatever saturated routes produce
  -- into a regular WAI 'Application'.
  --
  -- If no route matches or matching routes all fail during appraisal, an
  -- error response gets generated. It is extremely simple, @text/plain@
  -- response with the status code and message repeated in the body.
  -- You can register your own 'handler', though.
  --
  dispatch :: (h -> Application) -> Dispatch h a -> Application
  dispatch run Dispatch{runDispatch} req = do
    let Env{..} = execState runDispatch initEnv

    let good = mapMaybe ($ req) routes
        best = reverse (sortOn snd good)

    let app = case best of
                (h, Suitable _):_ -> run h
                (_, reason):_     -> err reason
                []                -> err NotFound

    let mwstack' = mwstack . handlerMW run handlers . abortMiddleware

    mwstack' app req


  err :: Score -> Application
  err (BadRequest reason)  = respond status400 (Just reason)
  err NotFound             = respond status404 Nothing
  err MethodNotAllowed     = respond status405 Nothing
  err UpgradeRequired      = respond status426 Nothing
  err NotAcceptable        = respond status406 Nothing
  err LengthRequired       = respond status411 Nothing
  err UnsupportedMediaType = respond status415 Nothing
  err (Suitable _)         = error "BUG: errored out with a Suitable"


  respond :: Status -> Maybe Text -> Application
  respond st@Status{..} msg _ sink = sink $ responseLBS st hdr msg'
    where
      hdr  = [(hContentType, "text/plain")]

      msg' = case msg of
               Nothing -> cs (show statusCode) <> " " <> cs statusMessage
               Just m  -> cs m


  handlerMW :: (h -> Application) -> [(Int, Response -> h)] -> Middleware
  handlerMW run handlers app req sink = do
    app req \resp -> do
      let Status{..} = responseStatus resp

      case lookup statusCode handlers of
        Just fn -> run (fn resp) req sink
        Nothing -> sink resp


-- vim:set ft=haskell sw=2 ts=2 et:
