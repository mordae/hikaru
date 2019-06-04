{-|
Module      :  Web.Hikaru.Route
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides path matching with parameter extraction
as well as content negotiation through path quality scoring.
-}

module Web.Hikaru.Route
  (
  -- * Route Selection
    selectRoute
  , RouteResult(..)

  -- ** Path Matching
  , seg
  , arg
  , argWith
  , rest

  -- ** Route Scoring
  , score
  , vary

  -- *** Method
  , method
  , get
  , post
  , head
  , put
  , patch
  , delete

  -- *** Request Content
  , acceptContent
  , acceptForm
  , acceptJSON

  -- *** Response Content
  , offerContent
  , offerHTML
  , offerText
  , offerJSON
  , offerCharset
  , offerEncoding
  , offerLanguage

  -- ** Types
  , Route
  , Result(..)
  )
where
  import BasePrelude hiding (head, delete)

  import Data.ByteString (ByteString)
  import Data.String.Conversions
  import Data.Text (Text)
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Method (Method)
  import Network.Wai
  import Web.Hikaru.Media
  import Web.Hikaru.Types


  -- |
  -- 'Applicative' used to associate a handler with conditions,
  -- upon which to invoke it.
  --
  -- Frequent conditions include HTTP method and path matching,
  -- but can also include an arbitrary 'Request' inspection
  --
  newtype Route a
    = Route
      { runRoute       :: Env -> (Env, Maybe a)
      }

  instance Functor Route where
    fmap f r = Route \env -> fmap (fmap f) (runRoute r env)

  instance Applicative Route where
    pure x = Route \env -> (env, Just x)

    (<*>) rf rx = Route \env ->
      case runRoute rf env of
        (env', Just f)  -> runRoute (fmap f rx) env'
        (env', Nothing) -> case runRoute rx env' of
                             (env'', _) -> (env'', Nothing)


  -- |
  -- Internal 'Route' environment.
  --
  data Env
    = Env
      { envPath        :: [Text]
        -- ^ Remaining path segments to consume during matching.

      , envScoring     :: [Request -> Result]
        -- ^ Registered scoring functions to select the best handler.

      , envVary        :: [HeaderName]
        -- ^ List of headers used to score this route.
      }


  -- |
  -- Route matching result.
  --
  -- Results are ordered in the descending order of preference.
  --
  -- Route matching results in the worst result encountered in the route.
  -- Dispatcher then selects the route with the best result overall.
  -- Best result does not necessarily mean a successfull one.
  --
  data Result
    = Success !Float
    | Failure !RequestError Text
    deriving (Eq, Ord)


  -- |
  -- Concatenating two results produces the worse one.
  -- If both are a success, the quality is multiplied.
  --
  instance Semigroup Result where
    (Success x) <> (Success y) = Success (x * y)
    x <> y = max x y

  -- |
  -- Results form a monoid with Success as the neutral element.
  --
  instance Monoid Result where
    mempty = Success 1.0


  -- |
  -- Result of a route matching.
  --
  -- * 'RequestError' is used to indicate the reason of failure.
  --
  -- * Successfull value is annotated with headers used to select it
  --   and the final quality. You should send them to the user using
  --   the @Vary@ header.
  --
  data RouteResult a
    = RouteFailed
      { rresError      :: RequestError
      , rresMessage    :: Text
      , rresVary       :: [HeaderName]
      }
    | RouteSuccess
      { rresAction     :: a
      , rresQuality    :: Float
      , rresVary       :: [HeaderName]
      }

  -- |
  -- It forms a semigroup that selects the best result overall.
  --
  instance Semigroup (RouteResult a) where
    x@(RouteSuccess _ g _) <> y@(RouteSuccess  _ h _) = if g >= h then x else y
    x@(RouteSuccess _ _ _) <> (  RouteFailed  _ _ _) = x
    (  RouteFailed  _ _ _) <> y@(RouteSuccess _ _ _) = y
    x@(RouteFailed  g _ _) <> y@(RouteFailed  h _ _) = if g >= h then x else y


  -- |
  -- Given a list of available routes and a 'Request'
  -- return the action to take or give a reason for failure.
  --
  selectRoute :: [Route a] -> Request -> RouteResult a
  selectRoute rs req = choosePath req $ mapMaybe bind rs
    where bind = flip bindPath (pathInfo req)


  -- |
  -- Map action to path components.
  --
  bindPath :: Route a -> [Text] -> Maybe (Request -> RouteResult a)
  bindPath rt p = case runRoute (rt <* end) (Env p [] []) of
                    (_,   Nothing) -> Nothing
                    (env, Just x)  -> Just (scoreResult env x)


  -- |
  -- Score the mapped action with respect to the request.
  --
  scoreResult :: Env -> a -> Request -> RouteResult a
  scoreResult Env{..} x req =
    case mconcat (map (req &) envScoring) of
      Failure exn msg -> RouteFailed exn msg envVary
      Success score'  -> if score' <= 0.0
                           then RouteFailed NotAcceptable "" envVary
                           else RouteSuccess x score' envVary


  -- |
  -- Select the best route with respect to the request.
  --
  choosePath :: Request -> [Request -> RouteResult a] -> RouteResult a
  choosePath req = choose . map (req &)
    where
      choose :: [RouteResult a] -> RouteResult a
      choose []     = RouteFailed NotFound "" []
      choose (r:rs) = sconcat (r :| rs)


  -- Path Matching -----------------------------------------------------------


  -- |
  -- Match and return following path segment.
  --
  -- Fails with 'NotFound' if the segment is missing or different.
  --
  seg :: Text -> Route Text
  seg s = argWith \t -> if s == t
                           then Just t
                           else Nothing


  -- |
  -- Match, parse and return following path segment.
  --
  -- Fails with 'NotFound' if the segment is missing or unparseable.
  --
  arg :: (FromParam a) => Route a
  arg = argWith fromParam


  -- |
  -- Match, parse and return next path segment using the supplied function.
  --
  -- Fails with 'NotFound' if the segment is missing.
  -- Otherwise respects the result of the matcher function.
  --
  argWith :: (Text -> Maybe a) -> Route a
  argWith match =
    Route \env@Env{..} ->
      case envPath of
        []     -> (env, Nothing)
        (x:xs) -> (env { envPath = xs }, match x)


  -- |
  -- Match and return all remaining path segments.
  --
  rest :: Route [Text]
  rest = Route \env@Env{..} -> (env { envPath = [] }, Just envPath)


  -- |
  -- Match end of the path (i.e. no remaining segments).
  -- Used to ensure that the path has been exhausted.
  --
  -- Fails with 'NotFound' if more segments are remaining.
  --
  end :: Route ()
  end = Route \env@Env{..} ->
    case envPath of
      [] -> (env, Just ())
      _  -> (env, Nothing)


  -- |
  -- Score route with respect to the request using the supplied function.
  --
  score :: (Request -> Result) -> Route ()
  score fn =
    Route \env@Env{..} ->
      (env { envScoring = fn : envScoring }, Just ())


  -- |
  -- Add list of headers that have been inspected in order to
  -- select proper response format.
  --
  -- This is needed to ensure our responses are cached properly by any
  -- proxies along the way, but you only need to use this function if
  -- your action performs some kind of additional content negotiation.
  -- All the scoring functions in this module do this automatically.
  --
  vary :: [HeaderName] -> Route ()
  vary hs =
    Route \env@Env{..} ->
      (env { envVary = hs <> envVary }, Just ())


  -- Methods -----------------------------------------------------------------


  -- |
  -- Match a particular HTTP method.
  --
  -- Fails with 'MethodNotAllowed' if a different method was used.
  --
  method :: Method -> Route ()
  method meth = score \req ->
    if meth == requestMethod req
       then Success 1.0
       else Failure MethodNotAllowed ""


  -- |
  -- Same as 'method' with the @GET@ argument.
  --
  get :: Route ()
  get = method "GET"


  -- |
  -- Same as 'method' with the @POST@ argument.
  --
  post :: Route ()
  post = method "POST"


  -- |
  -- Same as 'method' with the @HEAD@ argument.
  --
  head :: Route ()
  head = method "HEAD"


  -- |
  -- Same as 'method' with the @PUT@ argument.
  --
  put :: Route ()
  put = method "PUT"


  -- |
  -- Same as 'method' with the @PATCH@ argument.
  --
  patch :: Route ()
  patch = method "PATCH"


  -- |
  -- Same as 'method' with the @DELETE@ argument.
  --
  delete :: Route ()
  delete = method "DELETE"


  -- |
  -- Check that the content sent by the client is among the listed
  -- media types and fail with 'UnsupportedMediaType' if not.
  -- Adds @Vary: Content-Type@.
  --
  acceptContent :: [Media] -> Route ()
  acceptContent media =
    vary [hContentType] <* score \req ->
      let header = parseMedia (cs $ getContentType req)
       in case selectMedia media header of
            Nothing -> Failure UnsupportedMediaType ""
            Just md -> Success (mediaQuality md)


  -- |
  -- Shortcut to accept only form submissions.
  --
  acceptForm :: Route ()
  acceptForm = acceptContent [ "application/x-www-form-urlencoded"
                             , "multipart/form-data"
                             ]


  -- |
  -- Shortcut to accept only JSON documents.
  --
  acceptJSON :: Route ()
  acceptJSON = acceptContent [ "application/json"
                             , "text/json"
                             ]


  -- |
  -- Check that we can send an acceptable response to the client and
  -- fail with 'NotAcceptable' if not. Add @Vary: Accept@.
  --
  offerContent :: [Media] -> Route ()
  offerContent media =
    vary [hAccept] <* score \req ->
      let header = parseMedia (cs $ getAccept req)
       in case selectMedia media header of
            Nothing -> Failure NotAcceptable ""
            Just md -> Success (mediaQuality md)


  -- |
  -- Shortcut to offer HTML replies only.
  --
  offerHTML :: Route ()
  offerHTML = offerContent ["text/html"]


  -- |
  -- Shortcut to offer plain text replies only.
  --
  offerText :: Route ()
  offerText = offerContent ["text/plain"]


  -- |
  -- Shortcut to offer JSON replies only.
  --
  offerJSON :: Route ()
  offerJSON = offerContent ["application/json"]


  -- |
  -- Check that we can send an acceptable charset to the client and
  -- fail with 'NotAcceptable' if not. Add @Vary: Accept-Charset@.
  --
  offerCharset :: [Media] -> Route ()
  offerCharset media =
    vary [hAcceptCharset] <* score \req ->
      let header = parseMedia (cs $ getAcceptCharset req)
       in case selectMedia media header of
            Nothing -> Failure NotAcceptable ""
            Just md -> Success (mediaQuality md)


  -- |
  -- Check that we can send an acceptable encoding to the client and
  -- fail with 'NotAcceptable' if not. Add @Vary: Accept-Encoding@.
  --
  offerEncoding :: [Media] -> Route ()
  offerEncoding media =
    vary [hAcceptEncoding] <* score \req ->
      let header = parseMedia (cs $ getAcceptEncoding req)
       in case selectMedia media header of
            Nothing -> Failure NotAcceptable ""
            Just md -> Success (mediaQuality md)


  -- |
  -- Check that we can send an acceptable language to the client and
  -- fail with 'NotAcceptable' if not. Add @Vary: Accept-Language@.
  --
  offerLanguage :: [Media] -> Route ()
  offerLanguage media =
    vary [hAcceptLanguage] <* score \req ->
      let header = parseMedia (cs $ getAcceptLanguage req)
       in case selectMedia media header of
            Nothing -> Failure NotAcceptable ""
            Just md -> Success (mediaQuality md)



  -- Request Utilities -------------------------------------------------------

  -- |
  -- Obtain a specific request header.
  --
  getHeader :: HeaderName -> Request -> Maybe ByteString
  getHeader n req = lookup n (requestHeaders req)


  -- |
  -- Obtain the Accept header value or the default value of \"*/*\".
  --
  getAccept :: Request -> ByteString
  getAccept = fromMaybe "*/*" . getHeader hAccept


  -- |
  -- Obtain the Accept-Charset header value or the default value of \"*\".
  --
  getAcceptCharset :: Request -> ByteString
  getAcceptCharset = fromMaybe "*" . getHeader hAcceptCharset


  -- |
  -- Obtain the Accept-Encoding header value or the default
  -- value of \"identity,*;q=0\".
  --
  getAcceptEncoding :: Request -> ByteString
  getAcceptEncoding = fromMaybe "identity,*;q=0" . getHeader hAcceptEncoding


  -- |
  -- Obtain the Accept-Language header value or the default value of \"*\".
  --
  getAcceptLanguage :: Request -> ByteString
  getAcceptLanguage = fromMaybe "*/*" . getHeader hAcceptLanguage


  -- |
  -- Obtain the Content-Type header value or the default value of @\"\"@.
  --
  getContentType :: Request -> ByteString
  getContentType = fromMaybe "" . getHeader hContentType


-- vim:set ft=haskell sw=2 ts=2 et:
