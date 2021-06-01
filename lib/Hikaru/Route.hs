-- |
-- Module      :  Hikaru.Route
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides means for route construction.
--

module Hikaru.Route
  ( -- * Path Matching
    Route
  , root
  , (/:)
  , (//)
  , (/?)

    -- * Route Scoring
  , Appraisal(..)
  , Score(..)

    -- ** Method (HTTP Verb)
  , method
  , get
  , post

    -- ** WebSockets
  , requireWebsocket
  , websocket

    -- ** Request Content
  , acceptContent
  , acceptForm
  , acceptJSON

    -- ** Response Content
  , offerContent
  , offerHTML
  , offerText
  , offerJSON
  , offerEncoding
  , offerLanguage

    -- * Applying Routes
  , routePath
  , PathInfo(..)
  , routeLink
  , routeLinkHVect
  , routeApply
  , routeScore
  , routeVary
  )
where
  import Praha hiding (curry)
  import Hikaru.Media
  import Hikaru.Types

  import Network.HTTP.Types.Header
  import Network.Wai
  import Network.Wai.Handler.WebSockets

  import Data.List (reverse, nub, lookup)
  import Data.Typeable (TypeRep, typeRep)

  import Data.HVect hiding (reverse)


  -- |
  -- Route combines path description with a handler that gets saturated
  -- with captured path components.
  --
  -- Just make a path like this:
  --
  -- @
  -- getHelloR :: 'Route' \'[Text, Text] Text
  -- getHelloR = 'get' handler '//' \"hello\" '/:' \"greeting\" '/:' \"name\"
  --   where handler greeting name = mconcat [ greeting, \", \", name, \"!\" ]
  -- @
  --
  -- And then apply it to a list of path components, like that:
  --
  -- >>> routeApply ["hello", "What a nice day", "dear Reader"] getHelloR
  -- Just "What a nice day, dear Reader!"
  --
  -- Or construct a link to it with 'routeLink':
  --
  -- >>> href getHelloR "Ahoy" "Sailor"
  -- ["hello", "Ahoy", Sailor"]
  --
  -- Or better yet, use 'Hikaru.Dispatch.dispatch' to select the best
  -- route to handle a 'Request'.
  --
  data Route ts a
    = Route
      { path           :: [PathInfo]
      , func           :: [Text] -> Maybe a
      , score          :: [Request -> Score]
      , vary           :: [HeaderName]
      }
    deriving (Generic)

  instance NFData (Route ts a)


  data PathInfo
    = Segment Text
      -- ^ Path segment to be matched.
    | Capture Text TypeRep
      -- ^ Path segment to be captured.
    deriving (Show, Eq, Generic)

  instance NFData PathInfo


  -- |
  -- Indicates suitability as well as quality of a route.
  --
  -- Scores form a monoid, added together the smaller one wins.
  -- When two Suitable ones are added, their quality gets multiplied.
  -- Since the default quality is 1.0 and quality (when negotiating content)
  -- should be in the \((0.0, 1.0]\) range, this helps to select appropriate
  -- route across multiple appraisals.
  --
  data Score
    = BadRequest
    | NotFound
    | MethodNotAllowed
    | UpgradeRequired
    | NotAcceptable
    | LengthRequired
    | UnsupportedMediaType
    | Suitable Float
      -- ^ Suitable routes have quality.
    deriving (Show, Eq, Ord, Generic)

  instance NFData Score

  instance Semigroup Score where
    (Suitable x) <> (Suitable y) = Suitable (x * y)
    x <> y = min x y

  instance Monoid Score where
    mempty = Suitable 1.0


  -- |
  -- Appraisal of a route to decide whether it's suitable for a 'Request'
  -- or not.
  --
  -- Some appraisals (especially those for content negotiation) inspect
  -- request headers. HTTP spec mandates that any 'Response' that serves
  -- different content based on headers (e.g. in a different language) to
  -- always include a @Vary@ header with names of all headers used in this
  -- manner. You can get the list for a route using 'routeVary'.
  --
  -- You should mention those headers when designing your own appraisals,
  -- by the way.
  --
  data Appraisal
    = Appraisal
      { score          :: Request -> Score
      , vary           :: [HeaderName]
      }
    deriving (Generic)

  instance NFData Appraisal


  type family RouteElim (r :: Type) (ts :: [Type]) where
    RouteElim r (t ': ts) = t -> RouteElim r ts
    RouteElim r '[]       = r


  infixl 4 /:

  -- |
  -- Extends the route with a captured component that gets automatically
  -- converted using 'fromParam' behind the scenes when saturating
  -- the handler.
  --
  -- The 'Text' argument itself can be obtained using 'routePath' later,
  -- when you want to introspect the route for some reason. It serves no
  -- other purpose.
  --
  (/:) :: forall ts a b. (Param a, Typeable a)
       => Route ts (a -> b) -> Text -> Route (Reverse (a ': Reverse ts)) b
  (/:) r@Route{..} name = r { path = capture : path
                            , func = apply
                            }
    where
      capture = Capture name (typeRep proxy)
      proxy   = Proxy :: Proxy a

      apply (this:rest) = func rest <*> fromParam this
      apply _otherwise  = Nothing


  infixl 4 //

  -- |
  -- Extends the route with a matched component.
  --
  (//) :: Route ts a -> Text -> Route ts a
  (//) r@Route{..} seg = r { path = Segment seg : path
                           , func = apply
                           }
    where
      apply (this:rest) = if this == seg then func rest else Nothing
      apply _otherwise  = Nothing


  infixl 4 /?


  -- |
  -- Extends the route with an appraisal. That is, an additional condition
  -- (apart from path) that the route must satisfy in order to be used when
  -- dispatching or with 'routeScore'.
  --
  (/?) :: Route ts a -> Appraisal -> Route ts a
  (/?) r@Route{..} Appraisal{score = score', vary = vary'} =
    r { vary  = nub (vary' <> vary)
      , score = score' : score
      }


  -- |
  -- Ties an empty path to an unsaturated handler.
  --
  root :: handler -> Route '[] handler
  root x = Route { path  = []
                 , score = []
                 , vary  = []
                 , func  = \case
                             []    -> Just x
                             _else -> Nothing
                 }


  -- |
  -- Check that the request used given HTTP verb.
  --
  method :: ByteString -> Appraisal
  method verb = Appraisal {vary = [], score}
    where
      score req = if requestMethod req == verb
                     then Suitable 1.0
                     else MethodNotAllowed


  -- |
  -- Combines 'root' with 'method' for the most common case.
  --
  get :: handler -> Route '[] handler
  get fn = root fn /? method "GET"


  -- |
  -- Combines 'root' with 'method' for the second most common case.
  --
  post :: handler -> Route '[] handler
  post fn = root fn /? method "POST"


  -- |
  -- Check that the request wants to perform an upgrade to WebSocket.
  --
  -- Varies with 'hUpgrade'.
  --
  requireWebsocket :: Appraisal
  requireWebsocket = Appraisal {vary = [hUpgrade], score}
    where
      score req = if isWebSocketsReq req
                     then Suitable 1.0
                     else UpgradeRequired


  -- |
  -- Combines 'root' with 'requireWebsocket' for a common case.
  --
  websocket :: a -> Route '[] a
  websocket fn = root fn /? requireWebsocket


  -- |
  -- Return information about matched and captured path components
  -- for given route. Might come in handy for automated API docs.
  --
  routePath :: Route ts a -> [PathInfo]
  routePath Route{path} = reverse path


  -- |
  -- Apply route to a path, ideally resulting in a saturated handler.
  --
  routeApply :: [Text] -> Route ts a -> Maybe a
  routeApply xs Route{func} = func (reverse xs)


  -- |
  -- Score route for given 'Request'.
  --
  routeScore :: Request -> Route ts a -> Score
  routeScore req Route{score} = mconcat $ fmap ($ req) score


  -- |
  -- Get list of all headers 'routeScore' would use.
  --
  routeVary :: Route ts a -> [HeaderName]
  routeVary Route{vary} = vary


  -- |
  -- Construct link for the given route using supplied values of captured
  -- path components. Useful to create hrefs and form actions.
  --
  routeLink :: forall ts a. (HasRep ts, AllHave Param ts)
            => Route ts a -> HVectElim ts [Text]
  routeLink route = curry (routeLinkHVect route)


  -- |
  -- Same as 'routeLink', but operates on a heterogenous vector instead.
  -- Useful to create your own 'routeLink'-like functions.
  -- See 'curry' for more.
  --
  routeLinkHVect :: forall ts a. (AllHave Param ts)
                 => Route ts a -> HVect ts -> [Text]
  routeLinkHVect Route{path} = buildPath (reverse path)


  buildPath :: forall ts. (AllHave Param ts)
             => [PathInfo] -> HVect ts -> [Text]
  buildPath [] _ = []
  buildPath (Segment seg : more) xs = seg : buildPath more xs
  buildPath (Capture _ _ : more) (x :&: xs) = toParam x : buildPath more xs
  buildPath _ HNil = error "BUG: buildPath ran out of captures"


  -- |
  -- Check that the content sent by the client is among the listed
  -- media types and fail with 'UnsupportedMediaType' if not.
  --
  -- Varies with 'hContentType'.
  --
  acceptContent :: [Media] -> Appraisal
  acceptContent media = Appraisal {vary = [hContentType], score}
    where
      score req = do
        let header = parseMedia (cs $ getContentType req)

        case selectMedia media header of
          Just Media{..} -> Suitable quality
          Nothing        -> UnsupportedMediaType


  -- |
  -- Shortcut to accept only form submissions.
  --
  -- @
  -- acceptForm = acceptContent [ \"application/x-www-form-urlencoded\"
  --                            , \"multipart/form-data\"
  --                            ]
  -- @
  --
  acceptForm :: Appraisal
  acceptForm = acceptContent [ "application/x-www-form-urlencoded"
                             , "multipart/form-data"
                             ]


  -- |
  -- Shortcut to accept only JSON documents.
  --
  -- @
  -- acceptJSON = acceptContent [ \"application/json\"
  --                            , \"text/json\"
  --                            ]
  -- @
  --
  acceptJSON :: Appraisal
  acceptJSON = acceptContent [ "application/json"
                             , "text/json"
                             ]


  -- |
  -- Check that we can send an acceptable response to the client and
  -- fail with 'NotAcceptable' if not.
  --
  -- Varies with 'hAccept'.
  --
  offerContent :: [Media] -> Appraisal
  offerContent media = Appraisal {vary = [hAccept], score}
    where
      score req = do
        let header = parseMedia (cs $ getAccept req)

        case selectMedia media header of
          Just Media{..} -> Suitable quality
          Nothing        -> NotAcceptable


  -- |
  -- Shortcut to offer HTML replies only.
  --
  -- @
  -- offerHTML = offerContent [\"text/html\"]
  -- @
  --
  offerHTML :: Appraisal
  offerHTML = offerContent ["text/html"]


  -- |
  -- Shortcut to offer plain text replies only.
  --
  -- @
  -- offerText = offerContent [\"text/plain\"]
  -- @
  --
  offerText :: Appraisal
  offerText = offerContent ["text/plain"]


  -- |
  -- Shortcut to offer JSON replies only.
  --
  -- @
  -- offerJSON = offerContent [\"application/json\"]
  -- @
  --
  offerJSON :: Appraisal
  offerJSON = offerContent ["application/json"]


  -- |
  -- Check that we can send an acceptable encoding to the client and
  -- fail with 'NotAcceptable' if not.
  --
  -- Varies with 'hAcceptEncoding'.
  --
  offerEncoding :: [Media] -> Appraisal
  offerEncoding media = Appraisal {vary = [hAcceptEncoding], score}
    where
      score req = do
        let header = parseMedia (cs $ getAcceptEncoding req)

        case selectMedia media header of
          Just Media{..} -> Suitable quality
          Nothing        -> NotAcceptable


  -- |
  -- Check that we can send an acceptable language to the client and
  -- fail with 'NotAcceptable' if not.
  --
  -- Varies with 'hAcceptLanguage'.
  --
  offerLanguage :: [Media] -> Appraisal
  offerLanguage media = Appraisal {vary = [hAcceptLanguage], score}
    where
      score req = do
        let header = parseMedia (cs $ getAcceptLanguage req)

        case selectMedia media header of
          Just Media{..} -> Suitable quality
          Nothing        -> NotAcceptable


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
