{-|
Module      :  Web.Hikaru.Dispatch
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides monad-based request dispatching.
-}

module Web.Hikaru.Dispatch
  ( dispatch
  , route
  , wrapRoute
  , wrapAction
  , middleware
  , handler

  -- * Types
  , Dispatch
  , Nested
  , TopLevel
  )
where
  import BasePrelude hiding (insert, lookup, app)
  import qualified BasePrelude

  import Control.Monad.State (State, modify, execState)
  import Data.CaseInsensitive (original)
  import Data.Map.Strict hiding (map)
  import Data.Text (Text)
  import Network.HTTP.Types.Header
  import Network.Wai
  import Web.Hikaru.Route
  import Web.Hikaru.Types


  -- |
  -- Monad for the 'dispatch' function.
  --
  newtype Dispatch r l a
    = Dispatch
      { unDispatch     :: State (Env r) a
      }
    deriving (Functor, Applicative, Monad)


  -- |
  -- Part of the top-level 'Dispatch' signature.
  --
  data TopLevel


  -- |
  -- Part of the nested 'Dispatch' signature.
  --
  data Nested


  data Env r
    = Env
      { envRouteW      :: Route r -> Route r
      , envActionW     :: r -> r
      , envHandlers    :: Map RequestError (RequestError -> Text -> r)
      , envMiddleware  :: Middleware
      , envRoutes      :: [Route r]  -- in reverse order
      }


  initEnv :: Env r
  initEnv = Env id id mempty id mempty


  -- |
  -- Create an application out of routes, handlers and middleware.
  --
  -- You can use 'route', 'handler', 'middleware' to register each of
  -- these components respectively.
  --
  -- @
  -- app :: Application
  -- app = dispatch runAction $ do
  --   'middleware' $ logStdoutDev
  --
  --   'route' $ getRootR  \<$ 'get'
  --   'route' $ getHelloR \<$ 'get' <* 'seg' "hello" \<*\> 'arg'
  --
  --   'wrapRoute' needAuth $ do
  --     'route' $ getAdminR \<$ 'get'  \<* 'seg' "admin"
  --     'route' $ postPassR \<$ 'post' \<* 'seg' "admin" \<* 'seg' "password"
  --
  --   'handle' 'NotFound' notFoundR
  -- @
  --
  dispatch :: forall r. (r -> Application)
           -> forall a. Dispatch r TopLevel a
           -> Application
  dispatch runner = build runner . flip execState initEnv . unDispatch


  build :: forall r. (r -> Application) -> Env r -> Application
  build runner Env{..} = envMiddleware app'
    where
      app' req resp = mw app req resp
        where
          (mw, app) = case selectRoute (reverse envRoutes) req of
                        RouteFailed exn msg vhs -> (addVary vhs, err exn msg)
                        RouteSuccess ac _qu vhs -> (addVary vhs, run ac)

          run :: r -> Application
          run x req' resp' = catch (runner x req' resp')
                                   (\(exn, msg) -> mw (err exn msg) req' resp')

          err :: RequestError -> Text -> Application
          err exn msg = case lookup exn envHandlers of
                          Just eh -> runner (eh exn msg)
                          Nothing -> defaultHandler exn msg


  -- |
  -- Make sure that the @Vary@ response header contains specified names.
  --
  addVary :: [HeaderName] -> Middleware
  addVary vs = if vs == [] then id else apply
    where
      apply = modifyResponse (mapResponseHeaders fixup)
      fixup = modifyHeader "Vary" (maybe value (<> ", " <> value))
      value = mconcat $ intersperse ", " $ map original vs

      modifyHeader n fn hs = (n, v') : deleteBy headerEq (n, v') hs
        where v' = fn (BasePrelude.lookup n hs)

      headerEq (x, _) (y, _) = x == y


  -- |
  -- Register a route.
  --
  -- When multiple routes match with the same quality coefficient,
  -- the one registered first will be selected.
  --
  route :: Route r -> Dispatch r l ()
  route rt = Dispatch do
    modify \env@Env{..} ->
      env { envRoutes = envRouteW (fmap envActionW rt) : envRoutes }


  -- |
  -- Wrap all nested routes with a route transformer.
  --
  -- It is a little bit similar to the middleware, but route-specific and
  -- with full access to the routing utilities. Can be used e.g. to apply
  -- authentication to multiple routes at once or to update their content
  -- negotiation parameters.
  --
  wrapRoute :: (Route r -> Route r) -> Dispatch r Nested a -> Dispatch r l ()
  wrapRoute wrapper disp = Dispatch do
    modify \env ->
      let env' = execState (unDispatch disp)
                           (env { envRouteW = envRouteW env . wrapper })

       in env { envRoutes = envRoutes env' <> envRoutes env }


  -- |
  -- Wrap all nested actions with an action transformer.
  --
  --
  --
  wrapAction :: (r -> r) -> Dispatch r Nested a -> Dispatch r l ()
  wrapAction wrapper disp = Dispatch do
    modify \env ->
      let env' = execState (unDispatch disp)
                           (env { envActionW = envActionW env . wrapper })
       in env { envRoutes = envRoutes env' <> envRoutes env }


  -- |
  -- Register a middleware.
  --
  -- They are applied in the reverse order of their registation,
  -- i.e. the first one to be registered is the last one to be applied.
  --
  -- Middleware can only be registered at the top level of the dispatcher.
  --
  middleware :: Middleware -> Dispatch r TopLevel ()
  middleware mw = Dispatch do
    modify \env@Env{envMiddleware} ->
      env { envMiddleware = envMiddleware . mw }


  -- |
  -- Register a 'RequestError' handler.
  --
  -- The handler is passed the original request and is supposed to
  -- inform the user about the issue. It should use the correct HTTP
  -- status code.
  --
  -- If you need to perform content negotiation in the handler,
  -- you can use 'dispatch' in it as well. Just make sure to provide a
  -- route that cannot fail.
  --
  handler :: RequestError
          -> (RequestError -> Text -> r)
          -> Dispatch r TopLevel ()
  handler e h = Dispatch do
    modify \env@Env{envHandlers} ->
      env { envHandlers = insert e h envHandlers }


-- vim:set ft=haskell sw=2 ts=2 et:
