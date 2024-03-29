-- |
-- Module      :  Hikaru.Action
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides a monad for reacting to user requests by
-- building responses.
--

module Hikaru.Action
  ( MonadAction(..)
  , ActionT
  , runActionT

  -- ** Inspecting Request
  , getRequest
  , getMethod
  , getHeaders
  , getHeaderMaybe
  , getHeaderDefault
  , getBasicAuth
  , getAccept
  , getAcceptCharset
  , getAcceptEncoding
  , getAcceptLanguage
  , getContentType
  , getPathInfo
  , getPathInfoRaw
  , getParams
  , getParamMaybe
  , getParamList
  , getParamDefault
  , getCookies
  , getCookieMaybe
  , getCookieDefault
  , getReferrer
  , checkSameOrigin
  , getBodyLength
  , setBodyLimit
  , getBodyLimit
  , getBodyChunk
  , getBodyChunkIO
  , getBodyRaw
  , getJSON
  , getBody
  , FormData
  , getFormData
  , getFields
  , getFieldMaybe
  , getFieldDefault
  , getFieldList
  , getFiles
  , getFileMaybe
  , getFileList

  -- ** Building Response
  , setStatus
  , setHeaders
  , setHeader
  , addHeader
  , defaultHeader
  , modifyHeader
  , setCookie
  , setCookieEx
  , sendHTML
  , sendText
  , sendString
  , sendJSON
  , redirect
  , redirectBack
  , setResponseFile
  , setResponseBuilder
  , setResponseBS
  , setResponseBS'
  , setResponseText
  , setResponseText'
  , setResponseString
  , setResponseStream
  , setResponseRaw
  , done

  -- ** WebSockets
  , setFrameLimit
  , setMessageLimit
  , setResponseWS
  , WebSocket
  , wsSendText
  , wsSendBinary
  , wsReceive

  -- ** Errors
  , abort
  , abortMiddleware

  -- ** Localization
  , getLanguage
  , setLanguage

  -- ** Cacheing
  , withCache
  , dropCache
  , dropCaches

  -- ** Finalizing
  , registerFinalizer

  -- ** Action Environment
  , ActionEnv
  , respond

  -- ** Re-Exports
  , FilePath
  )
where
  import Crude.Prelude

  import Hikaru.HTML (fromHtmlT, HtmlT)
  import Hikaru.Media
  import Hikaru.Types

  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Lazy as LBS
  import qualified Data.Map.Strict as Map
  import qualified Data.Text.Lazy as LT
  import qualified Network.Wai.Parse as Parse

  import Control.Monad.Trans.Resource
  import Data.Aeson
  import Data.Binary.Builder
  import Data.ByteString.Char8 (words, span, drop)
  import Data.CaseInsensitive (mk)
  import Data.Dynamic
  import Data.List (deleteBy, lookup, map, filter)
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Method
  import Network.HTTP.Types.Status
  import Network.URI
  import Network.Wai
  import Network.Wai.Handler.WebSockets
  import OpenSSL.EVP.Base64
  import System.IO.Unsafe
  import UnliftIO
  import Web.Cookie

  import qualified Network.WebSockets as WS


  -- |
  -- 'MonadAction' provides access to the original 'Request' and means to
  -- build a 'Response' to send out.
  --
  -- * Request headers are always fully parsed.
  -- * Body is left untouched until you decide what to do with it.
  --
  -- Be careful not to blow out your memory usage by reading a multi-gigabyte
  -- attachment into a strict ByteString or something similar.
  --
  class (MonadIO m) => MonadAction m where
    -- |
    -- Return the action environment, including the 'Request' object,
    -- cached content from the user and the pending 'Response'.
    --
    askActionEnv :: m ActionEnv

    default askActionEnv
      :: (MonadTrans t, MonadAction n, m ~ t n) => m ActionEnv
    askActionEnv = lift askActionEnv
    {-# INLINE askActionEnv #-}

  -- |
  -- Allow access to action when building HTML responses.
  --
  instance (MonadAction m) => MonadAction (HtmlT m)


  -- |
  -- Simple monad transformer to add 'MonadAction' to a transformer
  -- stack. Makes use of 'ReaderT' under the wraps.
  --
  newtype ActionT m a
    = ActionT
      { stack          :: ReaderT ActionEnv m a
      }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadFail
             , Contravariant
             , MonadZip
             , Alternative
             , MonadPlus
             , MonadIO
             , MonadUnliftIO
             , MonadTrans
             )

  instance (MonadIO m) => MonadAction (ActionT m) where
    askActionEnv = ActionT ask
    {-# INLINE askActionEnv #-}


  runActionT :: ActionEnv -> ActionT m a -> m a
  runActionT env ActionT{stack} = runReaderT stack env
  {-# INLINE runActionT #-}


  -- |
  -- Exception raised by 'abort'.
  --
  data AbortAction
    = AbortAction
      { status         :: !Status
      , headers        :: [Header]
      , message        :: !Text
      }
    deriving (Show, Generic)

  instance NFData AbortAction where
    rnf AbortAction{status = Status{..}, ..} =
      rnf (statusCode, statusMessage, headers, message)

  instance Exception AbortAction


  -- |
  -- Obtain only the specific 'ActionEnv' field value.
  --
  getActionField :: (MonadAction m) => (ActionEnv -> IORef a) -> m a
  getActionField field = do
    ref <- field <$> askActionEnv
    readIORef ref


  -- |
  -- Set only the specific 'ActionEnv' field value.
  --
  setActionField :: (MonadAction m) => (ActionEnv -> IORef a) -> a -> m ()
  setActionField field value = do
    ref <- field <$> askActionEnv
    writeIORef ref value


  -- |
  -- Modify only the specific 'ActionEnv' field value.
  --
  modifyActionField :: (MonadAction m)
                    => (ActionEnv -> IORef a) -> (a -> a) -> m ()
  modifyActionField field fn = do
    ref <- field <$> askActionEnv
    modifyIORef' ref fn


  -- |
  -- Environment for the 'MonadAction'.
  --
  data ActionEnv
    = ActionEnv
      { aeRequest      :: Request
      , aeBody         :: !(IORef RequestBody)
      , aeRespStatus   :: !(IORef Status)
      , aeRespHeaders  :: !(IORef ResponseHeaders)
      , aeRespMaker    :: !(IORef ResponseMaker)
      , aeFinalize     :: !(IORef (IO ()))
      , aeBodyLimit    :: !(IORef Int64)
      , aeBodyCounter  :: !(IORef Int64)
      , aeLanguage     :: !(IORef Language)
      , aeCache        :: !(IORef (Map.Map Text Dynamic))
      , aeMsgLimit     :: !(IORef Int64)
      , aeFrameLimit   :: !(IORef Int64)
      }


  data Done = Done
    deriving (Show, Eq)

  instance Exception Done


  -- |
  -- Constructs 'ActionEnv' from the given 'Request', passes it to whatever
  -- action user deems interesting and finally constructs and send out the
  -- response from (ideally somewhat changed) data in the 'ActionEnv'.
  --
  -- Whole operation is bracketed to ensure all finalizers are run.
  --
  respond :: (ActionEnv -> IO ()) -> Application
  respond run req resp = do
    env <- makeActionEnv req

    bracket_ pass (finalize env) do
      catch (run env) \Done -> pass

      status  <- readIORef $ env.aeRespStatus
      headers <- readIORef $ env.aeRespHeaders
      make    <- readIORef $ env.aeRespMaker

      resp (make status headers)

    where
      finalize :: ActionEnv -> IO ()
      finalize = join . readIORef . (.aeFinalize)


  -- |
  -- Immediately stop building the response and return whatever
  -- status, headers and body has been set up so far.
  --
  done :: (MonadAction m) => m a
  done = throwIO Done
  {-# INLINE done #-}


  -- |
  -- Type of the function that, given status and headers, completes the
  -- 'Response' by producing a body.
  --
  type ResponseMaker = Status -> ResponseHeaders -> Response


  -- |
  -- Fields and files sent using a web form.
  --
  type FormData = ([(Text, Text)], [(Text, FilePath)])


  -- |
  -- Types of the request body.
  --
  data RequestBody
    = BodyUnparsed
      -- ^ Body has not yet been touched.
    | BodyTainted
      -- ^ Body has been partially consumed.
    | BodyForm FormData
      -- ^ Body has been successfully parsed as a form.
    | BodyJSON Value
      -- ^ Body has been successfully parsed as a JSON.
    | BodyBytes LBS.ByteString
      -- ^ Body has been successfully read in raw.
    | BodyWebSocket
      -- ^ Body is being used for WebSockets communication.


  -- |
  -- Create an initial action environment to handle given 'Request'.
  --
  makeActionEnv :: Request -> IO ActionEnv
  makeActionEnv req = do
    aeRequest     <- pure req
    aeBody        <- newIORef BodyUnparsed
    aeRespStatus  <- newIORef status200
    aeRespHeaders <- newIORef []
    aeRespMaker   <- newIORef (\st hs -> responseLBS st hs "")
    aeFinalize    <- newIORef pass
    aeBodyLimit   <- newIORef (10 * 1024 * 1024)
    aeBodyCounter <- newIORef 0
    aeLanguage    <- newIORef "en"
    aeCache       <- newIORef Map.empty
    aeMsgLimit    <- newIORef (1 * 1024 * 1024)
    aeFrameLimit  <- newIORef (1 * 1024 * 1024)

    return ActionEnv{..}


  -- Inspecting Request ------------------------------------------------------


  -- |
  -- Obtain the original 'Request'.
  --
  getRequest :: (MonadAction m) => m Request
  getRequest = (.aeRequest) <$> askActionEnv
  {-# INLINE getRequest #-}


  -- |
  -- Obtain the request method, such as @GET@ or @POST@.
  --
  getMethod :: (MonadAction m) => m Method
  getMethod = requestMethod <$> getRequest
  {-# INLINE getMethod #-}


  -- |
  -- Obtain the request headers.
  --
  getHeaders :: (MonadAction m) => m RequestHeaders
  getHeaders = requestHeaders <$> getRequest
  {-# INLINE getHeaders #-}


  -- |
  -- Obtain a specific request header.
  --
  getHeaderMaybe :: (MonadAction m) => HeaderName -> m (Maybe ByteString)
  getHeaderMaybe n = lookup n <$> getHeaders
  {-# INLINE getHeaderMaybe #-}


  -- |
  -- Obtain a specific request header or the given default value.
  --
  getHeaderDefault :: (MonadAction m)
                   => HeaderName -> ByteString -> m ByteString
  getHeaderDefault n v = fromMaybe v <$> getHeaderMaybe n
  {-# INLINE getHeaderDefault #-}


  -- |
  -- Obtain the login and password pair from the Authorization
  -- request header, if present.
  --
  getBasicAuth :: (MonadAction m) => m (Maybe (Text, Text))
  getBasicAuth = (parseBasicAuth =<<) <$> getHeaderMaybe "Authorization"
  {-# INLINE getBasicAuth #-}


  -- |
  -- Obtain the 'hAccept' header value or the default value of @\"*/*\"@.
  --
  -- Aborts with 'badRequest400' if the header fails to parse.
  --
  getAccept :: (MonadAction m) => m [Media]
  getAccept = do
    value <- getHeaderDefault hAccept "*/*"

    case parseMedia value of
      Left _reason -> abort badRequest400 [] "Failed to parse Accept."
      Right media  -> return media

  {-# INLINE getAccept #-}


  -- |
  -- Obtain the Accept-Charset header value or the default value of @\"*\"@.
  --
  -- Aborts with 'badRequest400' if the header fails to parse.
  --
  getAcceptCharset :: (MonadAction m) => m [Media]
  getAcceptCharset = do
    value <- getHeaderDefault hAcceptCharset "*"

    case parseMedia value of
      Left _reason -> abort badRequest400 [] "Failed to parse Accept-Charset."
      Right media  -> return media

  {-# INLINE getAcceptCharset #-}


  -- |
  -- Obtain the Accept-Encoding header value or the default
  -- value of @\"identity,*;q=0\"@.
  --
  -- Aborts with 'badRequest400' if the header fails to parse.
  --
  getAcceptEncoding :: (MonadAction m) => m [Media]
  getAcceptEncoding = do
    value <- getHeaderDefault hAcceptEncoding "identity,*;q=0"

    case parseMedia value of
      Left _reason -> abort badRequest400 [] "Failed to parse Accept-Encoding."
      Right media  -> return media

  {-# INLINE getAcceptEncoding #-}


  -- |
  -- Obtain the Accept-Language header value or the default value of @\"*\"@.
  --
  -- Aborts with 'badRequest400' if the header fails to parse.
  --
  getAcceptLanguage :: (MonadAction m) => m [Media]
  getAcceptLanguage = do
    value <- getHeaderDefault hAcceptLanguage "*"

    case parseMedia value of
      Left _reason -> abort badRequest400 [] "Failed to parse Accept-Language."
      Right media  -> return media

  {-# INLINE getAcceptLanguage #-}


  -- |
  -- Obtain the Content-Type header value or the default value of
  -- @\"application/octet-stream\"@ (always true, but meaningless).
  --
  -- Aborts with 'badRequest400' if the header fails to parse.
  --
  getContentType :: (MonadAction m) => m Media
  getContentType = do
    value <- getHeaderDefault hContentType "application/octet-stream"

    case parseMedia value of
      Left _reason -> abort badRequest400 [] "Failed to parse Content-Type."
      Right []     -> abort badRequest400 [] "Empty Content-Type."
      Right (m:_)  -> return m

  {-# INLINE getContentType #-}


  -- |
  -- Obtain request path split on forward slashes.
  --
  getPathInfo :: (MonadAction m) => m [Text]
  getPathInfo = pathInfo <$> getRequest
  {-# INLINE getPathInfo #-}


  -- |
  -- Obtain request path as an unsplit 'Text'.
  --
  getPathInfoRaw :: (MonadAction m) => m Text
  getPathInfoRaw = cs <$> rawPathInfo <$> getRequest
  {-# INLINE getPathInfoRaw #-}


  -- |
  -- Obtain all request query string parameters.
  --
  getParams :: (MonadAction m) => m [(Text, Text)]
  getParams = map convert <$> queryString <$> getRequest
    where convert (n, v) = (cs n, maybe "" cs v)

  {-# INLINE getParams #-}


  -- |
  -- Obtain a specific request query string parameter and parse it
  -- on the fly to the target type. Parsing failure maps to 'Nothing'.
  --
  getParamMaybe :: (MonadAction m, Param a) => Text -> m (Maybe a)
  getParamMaybe n = do
    value <- lookup n <$> getParams
    return $ fromParam =<< value

  {-# INLINE getParamMaybe #-}
  {-# SPECIALIZE getParamMaybe :: (MonadAction m) => Text -> m (Maybe Text) #-}
  {-# SPECIALIZE getParamMaybe :: (MonadAction m) => Text -> m (Maybe ByteString) #-}
  {-# SPECIALIZE getParamMaybe :: (MonadAction m) => Text -> m (Maybe Int32) #-}
  {-# SPECIALIZE getParamMaybe :: (MonadAction m) => Text -> m (Maybe Int64) #-}
  {-# SPECIALIZE getParamMaybe :: (MonadAction m) => Text -> m (Maybe Int) #-}


  -- |
  -- Similar to 'getParamMaybe', but return either the parsed parameter
  -- or the specified default value.
  --
  getParamDefault :: (MonadAction m, Param a) => Text -> a -> m a
  getParamDefault n v = fromMaybe v <$> getParamMaybe n
  {-# INLINE getParamDefault #-}
  {-# SPECIALIZE getParamDefault :: (MonadAction m) => Text -> Text -> m Text #-}
  {-# SPECIALIZE getParamDefault :: (MonadAction m) => Text -> ByteString -> m ByteString #-}
  {-# SPECIALIZE getParamDefault :: (MonadAction m) => Text -> Int32 -> m Int32 #-}
  {-# SPECIALIZE getParamDefault :: (MonadAction m) => Text -> Int64 -> m Int64 #-}
  {-# SPECIALIZE getParamDefault :: (MonadAction m) => Text -> Int -> m Int #-}


  -- |
  -- Obtain a group of request query string parameters with the same name
  -- and parse them on the fly to the target type.
  --
  getParamList :: (MonadAction m, Param a) => Text -> m [a]
  getParamList n = mapMaybe (fromParam . snd)
                   <$> filter ((n ==) . fst)
                   <$> getParams

  {-# INLINE getParamList #-}
  {-# SPECIALIZE getParamList :: (MonadAction m) => Text -> m [Text] #-}
  {-# SPECIALIZE getParamList :: (MonadAction m) => Text -> m [ByteString] #-}
  {-# SPECIALIZE getParamList :: (MonadAction m) => Text -> m [Int32] #-}
  {-# SPECIALIZE getParamList :: (MonadAction m) => Text -> m [Int64] #-}
  {-# SPECIALIZE getParamList :: (MonadAction m) => Text -> m [Int] #-}


  -- |
  -- Obtain all request cookies.
  --
  getCookies :: (MonadAction m) => m [(Text, Text)]
  getCookies = do
    mc <- getHeaderMaybe hCookie

    case mc of
      Nothing -> return []
      Just bs -> return $ map cs2 $ parseCookies bs

  {-# INLINE getCookies #-}


  -- |
  -- Obtain a specific cookie and parse it on the fly to the target type.
  -- Parsing failure maps to 'Nothing'.
  --
  getCookieMaybe :: (MonadAction m, Param a) => Text -> m (Maybe a)
  getCookieMaybe n = do
    value <- lookup n <$> getCookies
    return $ fromParam =<< value

  {-# INLINE getCookieMaybe #-}
  {-# SPECIALIZE getCookieMaybe :: (MonadAction m) => Text -> m (Maybe Text) #-}
  {-# SPECIALIZE getCookieMaybe :: (MonadAction m) => Text -> m (Maybe ByteString) #-}
  {-# SPECIALIZE getCookieMaybe :: (MonadAction m) => Text -> m (Maybe Int32) #-}
  {-# SPECIALIZE getCookieMaybe :: (MonadAction m) => Text -> m (Maybe Int64) #-}
  {-# SPECIALIZE getCookieMaybe :: (MonadAction m) => Text -> m (Maybe Int) #-}


  -- |
  -- Similar to 'getCookieMaybe', but return either the parsed cookie
  -- or the specified default value.
  --
  getCookieDefault :: (MonadAction m, Param a) => Text -> a -> m a
  getCookieDefault n v = fromMaybe v <$> getCookieMaybe n
  {-# INLINE getCookieDefault #-}
  {-# SPECIALIZE getCookieDefault :: (MonadAction m) => Text -> Text -> m Text #-}
  {-# SPECIALIZE getCookieDefault :: (MonadAction m) => Text -> ByteString -> m ByteString #-}
  {-# SPECIALIZE getCookieDefault :: (MonadAction m) => Text -> Int32 -> m Int32 #-}
  {-# SPECIALIZE getCookieDefault :: (MonadAction m) => Text -> Int64 -> m Int64 #-}
  {-# SPECIALIZE getCookieDefault :: (MonadAction m) => Text -> Int -> m Int #-}


  -- |
  -- Obtain HTTP @Referer@ header or just @/@.
  --
  -- Useful for redirects back to where the user came from.
  --
  getReferrer :: (MonadAction m) => m Text
  getReferrer = do
    header <- getHeaderDefault hReferer "/"
    return (cs header)

  {-# INLINE getReferrer #-}


  -- |
  -- Determine whether was the request made from the same origin or not.
  --
  -- Utilizes @Host@, @Origin@ and @Referer@ headers.
  --
  checkSameOrigin :: (MonadAction m) => m Bool
  checkSameOrigin = do
    host <- fmap cs <$> getHeaderMaybe "Host"
    origin <- fmap join $ fmap parseHost <$> getHeaderMaybe "Origin"
    referer <- fmap join $ fmap parseHost <$> getHeaderMaybe "Referer"

    return
      case (host, origin, referer) of
        (   Nothing,            _,             _) -> True
        (         _,      Nothing,       Nothing) -> True
        (Just host', Just origin',             _) -> host' == origin'
        (Just host',            _, Just referer') -> host' == referer'

  {-# INLINE checkSameOrigin #-}


  parseHost :: ByteString -> Maybe String
  parseHost uri = do
    case parseURI (cs uri) of
      Just URI{uriAuthority = Just URIAuth{uriRegName, uriPort}} ->
        Just (uriRegName <> uriPort)

      _otherwise -> Nothing


  -- |
  -- Try to obtain request body length.
  -- This will fail when the body is chunked.
  --
  getBodyLength :: (MonadAction m) => m (Maybe Int64)
  getBodyLength = do
    request <- getRequest

    case requestBodyLength request of
      ChunkedBody   -> return $ Nothing
      KnownLength n -> return $ Just (fromIntegral n)

  {-# INLINE getBodyLength #-}


  -- |
  -- Set limit (in bytes) for reading the request body in order to
  -- prevent memory exhaustion.
  --
  -- Default limit is 10 MiB, which is too little for any serious
  -- file storage and too much for simple CRUD applications.
  --
  -- The limit is enforced on two levels. First the 'getBodyLength'
  -- is consulted so that we can fail fast. Chunked requests are
  -- just read until the limit is exceeded.
  --
  -- Precise enforcement is more expensive, so we may allow slightly
  -- more (up to 32kb) than the limit set here when the request
  -- length is not known beforehand.
  --
  setBodyLimit :: (MonadAction m) => Int64 -> m ()
  setBodyLimit = setActionField (.aeBodyLimit)
  {-# INLINE setBodyLimit #-}


  -- |
  -- Return the payload size limit set by 'setBodyLimit'.
  --
  getBodyLimit :: (MonadAction m) => m Int64
  getBodyLimit = getActionField (.aeBodyLimit)
  {-# INLINE getBodyLimit #-}


  -- |
  -- Read next chunk of the body.
  --
  -- Returns 'Data.ByteString.empty' once the whole body has been consumed.
  --
  -- * Aborts with 'requestEntityTooLarge413' if reading next chunk would
  --   exceed the allotted request body limit. See 'setBodyLimit' for more.
  --
  getBodyChunk :: (MonadAction m) => m ByteString
  getBodyChunk = do
    getChunk <- getBodyChunkIO
    liftIO getChunk

  {-# INLINE getBodyChunk #-}


  -- |
  -- Return an IO action that will read next chunk of the body.
  --
  -- Returns 'Data.ByteString.empty' once the whole body has been consumed.
  --
  -- * Aborts with 'requestEntityTooLarge413' if reading next chunk would
  --   exceed the allotted request body limit. See 'setBodyLimit' for more.
  --
  getBodyChunkIO :: (MonadAction m) => m (IO ByteString)
  getBodyChunkIO = do
    limit    <- getActionField (.aeBodyLimit)
    counter  <- (.aeBodyCounter) <$> askActionEnv
    getChunk <- getRequestBodyChunk <$> getRequest

    return do
      haveRead <- readIORef counter

      if haveRead < limit
         then do
           chunk <- getChunk
           writeIORef counter $ haveRead + fromIntegral (BS.length chunk)
           return chunk

         else do
           abort requestEntityTooLarge413 []
                 ("Limit is " <> tshow limit <> " bytes.")


  -- |
  -- Obtain request body as a lazy 'LBS.ByteString'.
  --
  -- This uses lazy I/O under the surface, and therefore all typical
  -- warnings regarding lazy I/O apply. Namely, the resulting value may not
  -- outlive the request, because then it could be pointing to a connection
  -- that has already been closed.
  --
  -- Using this function directly will prevent access to the body in other
  -- ways, such as through the 'getJSON', 'getFields' or 'getFiles'.
  --
  -- * Aborts with 'requestEntityTooLarge413' if reading next chunk would
  --   exceed the allotted request body limit. See 'setBodyLimit' for more.
  --
  getBodyRaw :: (MonadAction m) => m LBS.ByteString
  getBodyRaw = do
    getChunk <- getBodyChunkIO
    liftIO $ LBS.fromChunks <$> generate getChunk

    where
      generate :: IO ByteString -> IO [ByteString]
      generate getChunk' = do
        chunk <- getChunk'
        more  <- unsafeInterleaveIO (generate getChunk')

        if chunk == ""
           then return []
           else return (chunk : more)


  -- |
  -- Read, parse, cache and return 'Value' sent by the user.
  --
  -- * Aborts with 'unsupportedMediaType415' if 'hContentType' does not
  --   indicate a JSON payload.
  --
  -- * Aborts with 'badRequest400' if the payload fails to parse.
  --
  -- * Aborts with 'requestEntityTooLarge413' if reading next chunk would
  --   exceed the allotted request body limit. See 'setBodyLimit' for more.
  --
  -- * Aborts with 'internalServerError500' if the body has already been
  --   consumed and was not cached as JSON.
  --
  getJSON :: (MonadAction m, FromJSON a) => m a
  getJSON = do
    -- First check out our stash.
    cache <- getActionField (.aeBody)

    case cache of
      -- This is ideal, we already have what we need.
      BodyJSON value ->
        case fromJSON value of
          Data.Aeson.Error err -> abort badRequest400 [] (cs err)
          Data.Aeson.Success out -> return out

      -- Body has not been parsed yet. This is very good.
      BodyUnparsed -> do
        ctype <- getContentType

        if matchMediaList ctype [ "application/json", "text/json" ]
           then pass
           else abort unsupportedMediaType415 [] "Send some JSON!"

        -- Taint and read.
        setActionField (.aeBody) BodyTainted
        body <- getBodyRaw

        -- Try to parse.
        value <- case eitherDecode' body of
                   Left reason -> abort badRequest400 [] (cs reason)
                   Right value -> return value

        -- Cache and return.
        setActionField (.aeBody) (BodyJSON value)

        -- Parse to the output type.
        case fromJSON value of
          Data.Aeson.Error err -> abort badRequest400 [] (cs err)
          Data.Aeson.Success out -> return out

      -- Now this is bad. We have already read the body,
      -- but not as a JSON. This is an internal error.
      _else -> do
        abort internalServerError500 [] "Body has been parsed as non-JSON."


  -- |
  -- Read, parse, cache and return form fields sent by the user.
  --
  -- If there were some files uploaded through the form as well,
  -- uploades them to a temporary location and caches information
  -- about them so that 'getFiles' can return them separately.
  --
  -- * Aborts with 'unsupportedMediaType415' if 'hContentType' does not
  --   indicate a form payload.
  --
  -- * Aborts with 'badRequest400' if the payload fails to parse.
  --
  -- * Aborts with 'requestEntityTooLarge413' if reading next chunk would
  --   exceed the allotted request body limit. See 'setBodyLimit' for more.
  --
  -- * Aborts with 'internalServerError500' if the body has already been
  --   consumed and was not cached as form data.
  --
  getFields :: (MonadAction m) => m [(Text, Text)]
  getFields = map cs2 <$> fst <$> getFormData
  {-# INLINE getFields #-}


  -- |
  -- Obtain a specific form field and parse it on the fly to the target type.
  -- Parsing failure maps to 'Nothing'.
  --
  getFieldMaybe :: (MonadAction m, Param a) => Text -> m (Maybe a)
  getFieldMaybe n = do
    value <- lookup n <$> getFields
    return $ fromParam =<< value

  {-# INLINE getFieldMaybe #-}
  {-# SPECIALIZE getFieldMaybe :: (MonadAction m) => Text -> m (Maybe Text) #-}
  {-# SPECIALIZE getFieldMaybe :: (MonadAction m) => Text -> m (Maybe ByteString) #-}
  {-# SPECIALIZE getFieldMaybe :: (MonadAction m) => Text -> m (Maybe Int32) #-}
  {-# SPECIALIZE getFieldMaybe :: (MonadAction m) => Text -> m (Maybe Int64) #-}
  {-# SPECIALIZE getFieldMaybe :: (MonadAction m) => Text -> m (Maybe Int) #-}


  -- |
  -- Similar to 'getFieldMaybe', but return either the parsed field
  -- or the specified default value.
  --
  getFieldDefault :: (MonadAction m, Param a) => Text -> a -> m a
  getFieldDefault n v = fromMaybe v <$> getFieldMaybe n
  {-# INLINE getFieldDefault #-}
  {-# SPECIALIZE getFieldDefault :: (MonadAction m) => Text -> Text -> m Text #-}
  {-# SPECIALIZE getFieldDefault :: (MonadAction m) => Text -> ByteString -> m ByteString #-}
  {-# SPECIALIZE getFieldDefault :: (MonadAction m) => Text -> Int32 -> m Int32 #-}
  {-# SPECIALIZE getFieldDefault :: (MonadAction m) => Text -> Int64 -> m Int64 #-}
  {-# SPECIALIZE getFieldDefault :: (MonadAction m) => Text -> Int -> m Int #-}


  -- |
  -- Obtain a group of form fields with the same name and parse them on the
  -- fly to the target type.
  --
  getFieldList :: (MonadAction m, Param a) => Text -> m [a]
  getFieldList n = mapMaybe (fromParam . snd)
                   <$> filter ((n ==) . fst)
                   <$> getFields

  {-# INLINE getFieldList #-}
  {-# SPECIALIZE getFieldList :: (MonadAction m) => Text -> m [Text] #-}
  {-# SPECIALIZE getFieldList :: (MonadAction m) => Text -> m [ByteString] #-}
  {-# SPECIALIZE getFieldList :: (MonadAction m) => Text -> m [Int32] #-}
  {-# SPECIALIZE getFieldList :: (MonadAction m) => Text -> m [Int64] #-}
  {-# SPECIALIZE getFieldList :: (MonadAction m) => Text -> m [Int] #-}


  -- |
  -- Identical to 'getFields', except it returns information about
  -- files uploaded through the form.
  --
  getFiles :: (MonadAction m) => m [(Text, FilePath)]
  getFiles = snd <$> getFormData
  {-# INLINE getFiles #-}


  -- |
  -- Obtain a specific form file
  --
  getFileMaybe :: (MonadAction m) => Text -> m (Maybe FilePath)
  getFileMaybe n = lookup n <$> getFiles
  {-# INLINE getFileMaybe #-}


  -- |
  -- Obtain a group of form files with the same name.
  --
  getFileList :: (MonadAction m) => Text -> m [FilePath]
  getFileList n = map snd . filter ((n ==) . fst) <$> getFiles
  {-# INLINE getFileList #-}


  -- |
  -- Backend for both 'getFields' and 'getFiles' that parses,
  -- caches and returns form data.
  --
  -- * Aborts with 'unsupportedMediaType415' if 'hContentType' does not
  --   indicate a form payload.
  --
  -- * Aborts with 'badRequest400' if the payload fails to parse.
  --
  -- * Aborts with 'requestEntityTooLarge413' if reading next chunk would
  --   exceed the allotted request body limit. See 'setBodyLimit' for more.
  --
  -- * Aborts with 'internalServerError500' if the body has already been
  --   consumed and was not cached as form data.
  --
  getFormData :: (MonadAction m) => m FormData
  getFormData = do
    cache <- getActionField (.aeBody)

    case cache of
      -- This is ideal, we already have what we need.
      BodyForm form -> return form

      -- Body has not been parsed yet. This is very good.
      BodyUnparsed -> do
        bodyType <- Parse.getRequestBodyType <$> getRequest
        getChunk <- getBodyChunkIO

        case bodyType of
          Nothing -> abort unsupportedMediaType415 [] "Send some form!"
          Just bt -> do
            -- Prepare for uploaded files finalization.
            rtis <- createInternalState
            registerFinalizer (closeInternalState rtis)

            -- Parse the form data.
            form' <- liftIO do
              Parse.sinkRequestBody (Parse.tempFileBackEnd rtis) bt getChunk

            -- Perform string conversions and simplify uploaded file types.
            let form = adaptForm form'

            -- Cache and return.
            setActionField (.aeBody) (BodyForm form)
            return form

      -- Now this is bad. We have already read the body,
      -- but not as a form. This is an internal error.
      _else -> do
        abort internalServerError500 [] "Body has been parsed as non-form."


  -- |
  -- Convert form names and fields from 'ByteString' to 'Text' and
  -- extract just the uploaded file names from the 'Parse.FileInfo' structures.
  --
  adaptForm :: ([Parse.Param], [(ByteString, Parse.FileInfo FilePath)]) -> FormData
  adaptForm (ps, fs) = (map cs2 ps, map convFile fs)
    where
      convFile (n, Parse.FileInfo{fileContent}) = (cs n, fileContent)


  -- |
  -- Read, cache and return payload sent by the user.
  --
  -- * Throws 'PayloadTooLarge' if the payload size limit is exceeded.
  --   Use 'setBodyLimit' to adjust the limit to your liking.
  --
  -- * Uses 'getBodyRaw' to get the payload meaning that the body size
  --   limit can get hit after you start processing the result.
  --
  getBody :: (MonadAction m) => m LBS.ByteString
  getBody = do
    cache <- getActionField (.aeBody)

    case cache of
      -- This is ideal, we already have what we need.
      BodyBytes bstr -> return bstr

      -- Body has not been parsed yet. This is very good.
      BodyUnparsed -> do
        -- Taint and read.
        setActionField (.aeBody) BodyTainted
        body <- getBodyRaw

        -- Force it whole.
        _len  <- LBS.length <$> pure body

        -- Cache and return.
        setActionField (.aeBody) (BodyBytes body)
        return body

      -- Now this is bad. We have already read the body,
      -- but not as a raw data. This is an internal error.
      _else -> do
        abort internalServerError500 [] "Body has already been parsed."


  -- Building Response -------------------------------------------------------


  -- |
  -- Set the status to use when building our 'Response'.
  --
  setStatus :: (MonadAction m) => Status -> m ()
  setStatus = setActionField (.aeRespStatus)
  {-# INLINE setStatus #-}


  -- |
  -- Set headers to use when building our 'Response'.
  --
  setHeaders :: (MonadAction m) => ResponseHeaders -> m ()
  setHeaders = setActionField (.aeRespHeaders)
  {-# INLINE setHeaders #-}


  -- |
  -- Append a single 'Response' header without checking.
  --
  addHeader :: (MonadAction m) => HeaderName -> ByteString -> m ()
  addHeader n v = modifyActionField (.aeRespHeaders) ((n, v) :)
  {-# INLINE addHeader #-}


  -- |
  -- Set a single 'Response' header to a new value.
  -- If the header has been given multiple times, leave only one.
  --
  setHeader :: (MonadAction m) => HeaderName -> ByteString -> m ()
  setHeader n v = modifyActionField (.aeRespHeaders) update
    where
      update hs = (n, v) : deleteBy headerEq (n, v) hs

  {-# INLINE setHeader #-}


  -- |
  -- Set header only if it has not been set yet.
  --
  -- Used by the 'sendHTML', 'sendJSON' and other similar functions.
  --
  defaultHeader :: (MonadAction m) => HeaderName -> ByteString -> m ()
  defaultHeader n v = modifyHeader n (fromMaybe v)
  {-# INLINE defaultHeader #-}


  -- |
  -- Replace a single 'Response' header with a new one that is constructed
  -- by applying the supplied function to the value of the previous one. Only
  -- the last header is modified, other matching headers are discarded.
  --
  -- Used in conjunction with 'maybe' this can be used to append header
  -- values in a sensible way.
  --
  -- @
  -- modifyHeader 'hVary' $ maybe "Accept" (<> ", Accept")
  -- @
  --
  modifyHeader :: (MonadAction m)
               => HeaderName -> (Maybe ByteString -> ByteString) -> m ()
  modifyHeader n fn = modifyActionField (.aeRespHeaders) update
    where
      update hs = (n, v') : deleteBy headerEq (n, v') hs
        where v' = fn (lookup n hs)

  {-# INLINE modifyHeader #-}


  -- |
  -- Set a cookie with name, value and @SameSite=Lax@.
  --
  -- Such cookies are valid for the whole domain, expire when the browser
  -- is closed, can be accessed from JavaScript and can not be sent with
  -- cross-site requests.
  --
  setCookie :: (MonadAction m, Param a) => Text -> a -> m ()
  setCookie name value = do
    setCookieEx $ defaultSetCookie { setCookieName     = cs name
                                   , setCookieValue    = cs (toParam value)
                                   , setCookiePath     = Just "/"
                                   , setCookieSameSite = Just sameSiteLax
                                   }

  {-# INLINE setCookie #-}


  -- |
  -- Set a cookie using the 'Web.Cookie.SetCookie' directly.
  --
  setCookieEx :: (MonadAction m) => SetCookie -> m ()
  setCookieEx cookie = do
    addHeader hSetCookie $ cs $ toLazyByteString $ renderSetCookie cookie

  {-# INLINE setCookieEx #-}


  -- |
  -- Default @Content-Type@ to @text/html; charset=utf8@
  -- and set the response body to the rendering of provided HTML markup.
  --
  sendHTML :: (MonadAction m) => HtmlT m a -> m ()
  sendHTML html = do
    defaultHeader hContentType "text/html; charset=utf8"
    builder <- fromHtmlT html
    setResponseBuilder builder

  {-# INLINE sendHTML #-}


  -- |
  -- Default @Content-Type@ to @text/plain; charset=utf8@
  -- and set the response body to the provided text.
  --
  sendText :: (MonadAction m) => Text -> m ()
  sendText text = do
    defaultHeader hContentType "text/plain; charset=utf8"
    setResponseText' text

  {-# INLINE sendText #-}


  -- |
  -- Default @Content-Type@ to @text/plain; charset=utf8@
  -- and set the response body to the provided string.
  --
  sendString :: (MonadAction m) => String -> m ()
  sendString str = do
    defaultHeader hContentType "text/plain; charset=utf8"
    setResponseString str

  {-# INLINE sendString #-}


  -- |
  -- Default @Content-Type@ to @application/json@ and set the response
  -- body to the result of encoding provided Aeson value.
  --
  sendJSON :: (MonadAction m, ToJSON a) => a -> m ()
  sendJSON payload = do
    defaultHeader hContentType "application/json"
    setResponseBS (encode payload)

  {-# INLINE sendJSON #-}


  -- |
  -- Set the response status to 303 (See Other), that will cause the browser
  -- to obtain the address specified in the supplied @Location@ header using
  -- the @GET@ method.
  --
  redirect :: (MonadAction m) => Text -> m ()
  redirect location = do
    setStatus status303
    setHeader hLocation (cs location)

  {-# INLINE redirect #-}


  -- |
  -- Redirect the user to where he came from, using 'getReferrer'.
  -- It defaults to @/@ if no @Referer@ header is present.
  --
  redirectBack :: (MonadAction m) => m ()
  redirectBack = redirect =<< getReferrer
  {-# INLINE redirectBack #-}


  -- |
  -- Create response body using a file.
  --
  -- Optional 'FilePath' argument allows for Range header support.
  --
  setResponseFile :: (MonadAction m) => FilePath -> Maybe FilePart -> m ()
  setResponseFile fp mfp = do
    setActionField (.aeRespMaker) \st hs -> responseFile st hs fp mfp

  {-# INLINE setResponseFile #-}


  -- |
  -- Create response body using a 'Builder'.
  --
  setResponseBuilder :: (MonadAction m) => Builder -> m ()
  setResponseBuilder bld = do
    setActionField (.aeRespMaker) \st hs -> responseBuilder st hs bld

  {-# INLINE setResponseBuilder #-}


  -- |
  -- Create response body using a lazy 'LBS.ByteString'.
  --
  setResponseBS :: (MonadAction m) => LBS.ByteString -> m ()
  setResponseBS bs = do
    setActionField (.aeRespMaker) \st hs -> responseLBS st hs bs

  {-# INLINE setResponseBS #-}


  -- |
  -- Create response body using a strict 'ByteString'.
  --
  setResponseBS' :: (MonadAction m) => ByteString -> m ()
  setResponseBS' = setResponseBS . cs

  {-# INLINE setResponseBS' #-}


  -- |
  -- Create response body using a lazy 'LT.Text'.
  --
  setResponseText :: (MonadAction m) => LT.Text -> m ()
  setResponseText = setResponseBS . cs

  {-# INLINE setResponseText #-}


  -- |
  -- Create response body using a strict 'Text'.
  --
  setResponseText' :: (MonadAction m) => Text -> m ()
  setResponseText' = setResponseBS . cs

  {-# INLINE setResponseText' #-}


  -- |
  -- Create response body using a 'String'.
  --
  setResponseString :: (MonadAction m) => String -> m ()
  setResponseString = setResponseBS . cs
  {-# INLINE setResponseString #-}


  -- |
  -- Create response body using a stream of values.
  --
  setResponseStream :: (MonadAction m) => StreamingBody -> m ()
  setResponseStream strm = do
    setActionField (.aeRespMaker) \st hs -> responseStream st hs strm

  {-# INLINE setResponseStream #-}


  -- |
  -- Create a raw response. This is useful for "upgrade" situations,
  -- where an application requests for the server to grant it raw
  -- network access.
  --
  -- This function requires a backup response to be provided, for the
  -- case where the handler in question does not support such upgrading.
  --
  -- Ignores both status and headers set so far. You need to emit these
  -- yourself, if needed.
  --
  -- Try not to read from the body before starting the raw response
  -- or risk encountering undefined behavior.
  --
  setResponseRaw :: (MonadAction m)
                 => (IO ByteString -> (ByteString -> IO ()) -> IO ())
                 -> Response
                 -> m ()
  setResponseRaw comm resp = do
    setActionField (.aeRespMaker) \_st _hs -> responseRaw comm resp

  {-# INLINE setResponseRaw #-}


  -- WebSockets --------------------------------------------------------------


  -- |
  -- Set limit (in bytes) for reading the individual WebSocket frames
  -- in order to prevent memory exhaustion.
  --
  -- Default limit is 1 MiB, which is too little for file transmission
  -- and too much for simple notifications. You might even consider
  -- lowering it down to e.g. 125 bytes for sockets that are supposed
  -- to communicate in one way only.
  --
  setFrameLimit :: (MonadAction m) => Int64 -> m ()
  setFrameLimit = setActionField (.aeFrameLimit)
  {-# INLINE setFrameLimit #-}


  -- |
  -- Set limit (in bytes) for reading the individual WebSocket messages
  -- in order to prevent memory exhaustion.
  --
  -- Default limit is 1 MiB, which is too little for file transmission
  -- and too much for simple notifications. You might even consider
  -- lowering it down to e.g. 125 bytes for sockets that are supposed
  -- to communicate in one way only.
  --
  -- Single message may or may not consist of multiple frames.
  --
  setMessageLimit :: (MonadAction m) => Int64 -> m ()
  setMessageLimit = setActionField (.aeMsgLimit)
  {-# INLINE setMessageLimit #-}


  -- |
  -- Attempt to upgrade the connection to a WebSocket.
  --
  -- The 'WebSocket' monad can be used to communicate with the client.
  --
  -- Sets up an automatic keep-alive with a 30s ping interval.
  --
  setResponseWS :: (MonadAction m) => WebSocket () -> m ()
  setResponseWS WebSocket{runWebSocket} = do
    -- First check the body situation.
    body <- getActionField (.aeBody)

    case body of
      BodyUnparsed -> do
        frameLimit   <- WS.SizeLimit <$> getActionField (.aeFrameLimit)
        messageLimit <- WS.SizeLimit <$> getActionField (.aeMsgLimit)

        let opts = WS.defaultConnectionOptions
                     { WS.connectionFramePayloadSizeLimit = frameLimit
                     , WS.connectionMessageDataSizeLimit  = messageLimit
                     }

        req <- getRequest

        setActionField (.aeBody) BodyWebSocket
        setActionField (.aeRespMaker) \_st _hs ->
          case websocketsApp opts app req of
            Nothing   -> responseLBS status400 [] "WebSocket Expected"
            Just resp -> resp

      _else -> do
        abort internalServerError500 [] "Body has already been consumed."

    where
      app :: WS.PendingConnection -> IO ()
      app pc = do
        void do
          conn <- WS.acceptRequest pc
          WS.withPingThread conn 30 pass do
            runReaderT runWebSocket conn


  -- |
  -- WebSocket context.
  --
  newtype WebSocket a
    = WebSocket
      { runWebSocket   :: ReaderT WS.Connection IO a
      }
    deriving (MonadUnliftIO, MonadIO, Monad, Applicative, Functor)


  -- |
  -- Send a textual message.
  --
  wsSendText :: (WS.WebSocketsData a) => a -> WebSocket ()
  wsSendText payload = do
    conn <- wsGetConn
    liftIO $ WS.sendTextData conn payload

  {-# INLINE wsSendText #-}


  -- |
  -- Send a binary message.
  --
  wsSendBinary :: (WS.WebSocketsData a) => a -> WebSocket ()
  wsSendBinary payload = do
    conn <- wsGetConn
    liftIO $ WS.sendBinaryData conn payload

  {-# INLINE wsSendBinary #-}


  -- |
  -- Receive a message decoded as either binary or text,
  -- depending on the requested value type.
  --
  wsReceive :: (WS.WebSocketsData a) => WebSocket a
  wsReceive = do
    conn <- wsGetConn
    liftIO $ WS.receiveData conn

  {-# INLINE wsReceive #-}


  -- |
  -- Get the WebSocket connection.
  --
  wsGetConn :: WebSocket WS.Connection
  wsGetConn = WebSocket ask
  {-# INLINE wsGetConn #-}


  -- Errors ------------------------------------------------------------------


  -- |
  -- Raise 'AbortAction' using given status, headers and a textual body.
  -- Headers are prefixed with ('hContentType', @text/plain@).
  --
  -- You are supposed to use it together with 'abortMiddleware' that turns
  -- such exception into regular 'Response's.
  --
  abort :: (MonadIO m) => Status -> [Header] -> Text -> m a
  abort st hdrs msg = throwIO $ AbortAction st hdrs' msg
    where hdrs' = (hContentType, "text/plain; charset=utf8") : hdrs

  {-# INLINE abort #-}


  -- |
  -- Catches 'AbortAction' and turns it into a 'Response'.
  --
  abortMiddleware :: Middleware
  abortMiddleware app req sink = do
    app req sink `catch` \AbortAction{..} -> do
      sink $ responseLBS status headers (cs message)

  {-# INLINE abortMiddleware #-}


  -- Localization ------------------------------------------------------------


  -- |
  -- Get the preferred language to be used for localization.
  --
  -- Language must be set using the 'setLanguage' function or through
  -- the localization tools found in the "Hikaru.Localize" module.
  --
  getLanguage :: (MonadAction m) => m Language
  getLanguage = getActionField (.aeLanguage)
  {-# INLINE getLanguage #-}


  -- |
  -- Set preferred language to be used for localization.
  --
  -- See 'getLanguage' above for more information.
  --
  setLanguage :: (MonadAction m) => Language -> m ()
  setLanguage = setActionField (.aeLanguage)
  {-# INLINE setLanguage #-}


  -- Cacheing ----------------------------------------------------------------


  -- |
  -- Run the effect only if it was not found in the cache.
  --
  -- The cache is request-specific and will be dropped after the request
  -- has been handled. It can be also dropped manually using 'dropCache'
  -- or 'dropCaches'.
  --
  -- The first time 'withCache' is called with a given key, the resulting
  -- value is stored in the cache under that key. Next time, the effect is
  -- not executed and the cached value is returned instead.
  --
  -- Since 'Dynamic' is used under the wraps, reusing the same key with a
  -- different type of value is safe and will result in overwriting the
  -- old key. Not very efficient to make use of it, though.
  --
  withCache :: (MonadAction m, Typeable a) => Text -> m a -> m a
  withCache key makeValue = do
    cache <- getActionField (.aeCache)

    case fromDynamic =<< Map.lookup key cache of
      Nothing -> do
        value <- makeValue
        modifyActionField (.aeCache) (Map.insert key (toDyn value))
        return value

      Just value -> do
        return value

  {-# INLINE withCache #-}


  -- |
  -- Drop a single cached value.
  --
  dropCache :: (MonadAction m) => Text -> m ()
  dropCache key = modifyActionField (.aeCache) (Map.delete key)
  {-# INLINE dropCache #-}


  -- |
  -- Drop all cached values.
  --
  dropCaches :: (MonadAction m) => m ()
  dropCaches = modifyActionField (.aeCache) (const Map.empty)
  {-# INLINE dropCaches #-}


  -- Finalizing --------------------------------------------------------------


  -- |
  -- Register an IO action to run once the request is either handler
  -- or fails with an error.
  --
  registerFinalizer :: (MonadAction m) => IO a -> m ()
  registerFinalizer fin = modifyActionField (.aeFinalize) (fin >>)
  {-# INLINE registerFinalizer #-}


  -- Misc Utilities ----------------------------------------------------------


  -- |
  -- Helper to compare two headers by their name.
  --
  headerEq :: (Eq a) => (a, b) -> (a, b) -> Bool
  headerEq (x, _) (y, _) = x == y


  -- |
  -- Helper to apply 'cs' to both elements of a 2-tuple.
  --
  cs2 :: (ConvertibleStrings a c, ConvertibleStrings b d)
      => (a, b) -> (c, d)
  cs2 (x, y) = (cs x, cs y)


  parseBasicAuth :: ByteString -> Maybe (Text, Text)
  parseBasicAuth value =
    case words value of
      [method, auth] | mk method == "Basic" -> do
        let lp = decodeBase64BS (cs auth)
            (l, p) = span (/= ':') lp
         in Just (cs l, cs (drop 1 p))

      _otherwise -> Nothing


-- vim:set ft=haskell sw=2 ts=2 et:
