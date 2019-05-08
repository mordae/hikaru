{-|
Module      :  Web.Hikaru.Action
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides a monad for reacting to user requests by
building responses.
-}

module Web.Hikaru.Action
  ( MonadAction(..)

  -- ** Inspecting Request
  , getRequest
  , getMethod
  , getHeaders
  , getHeaderMaybe
  , getHeaderDefault
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
  , getBodyLength
  , setBodyLimit
  , getBodyLimit
  , getBodyChunk
  , getBodyChunkIO
  , getBodyRaw
  , getJSON
  , getBody
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

  -- ** Errors
  , throwError

  -- ** Localization
  , setLanguages
  , getLanguages

  -- ** Finalizing
  , registerFinalizer

  -- ** Action Environment
  , ActionEnv
  , makeActionEnv
  , respond
  )
where
  import BasePrelude hiding (length)

  import qualified Data.ByteString.Lazy as LBS
  import qualified Data.Text.Lazy as LT

  import Control.Monad.Trans (lift)
  import Control.Monad.Trans.Resource
  import Data.Aeson (Value, ToJSON, encode, eitherDecode')
  import Data.Binary.Builder
  import Data.ByteString (ByteString, length)
  import Data.String.Conversions
  import Data.Text (Text)
  import Lucid
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Method
  import Network.HTTP.Types.Status
  import Network.Wai
  import Network.Wai.Internal (getRequestBodyChunk)
  import Network.Wai.Parse
  import Web.Cookie
  import Web.Hikaru.Media
  import Web.Hikaru.Types


  -- |
  -- Inside the 'MonadAction' the original 'Request' is available and we can
  -- query it for information while building a fresh 'Response' to send out.
  --
  -- * Request header is always fully parsed.
  -- * Body is left untouched until you decide what to do with it.
  --
  -- Be careful not to blow up your memory usage by reading a multi-gigabyte
  -- attachment into a strict ByteString or something.
  --
  class (MonadIO m) => MonadAction m where
    -- |
    -- Return the action environment, including the 'Request' object, cached
    -- content from the user and the pending 'Response'.
    --
    getActionEnv :: m ActionEnv

  -- |
  -- Allow access to action when building HTML responses.
  --
  instance (MonadAction m) => MonadAction (HtmlT m) where
    getActionEnv = lift getActionEnv


  -- |
  -- Obtain only the specific 'ActionEnv' field value.
  --
  getActionField :: (MonadAction m) => (ActionEnv -> IORef a) -> m a
  getActionField field = do
    ref <- field <$> getActionEnv
    liftIO $ readIORef ref


  -- |
  -- Set only the specific 'ActionEnv' field value.
  --
  setActionField :: (MonadAction m) => (ActionEnv -> IORef a) -> a -> m ()
  setActionField field value = do
    ref <- field <$> getActionEnv
    liftIO $ writeIORef ref value


  -- |
  -- Modify only the specific 'ActionEnv' field value.
  --
  modifyActionField :: (MonadAction m)
                    => (ActionEnv -> IORef a) -> (a -> a) -> m ()
  modifyActionField field fn = do
    ref <- field <$> getActionEnv
    liftIO $ modifyIORef' ref fn


  -- |
  -- Environment for the 'MonadAction'.
  --
  data ActionEnv
    = ActionEnv
      { aeRequest      :: Request
      , aeBody         :: IORef RequestBody
      , aeRespStatus   :: IORef Status
      , aeRespHeaders  :: IORef ResponseHeaders
      , aeRespMaker    :: IORef ResponseMaker
      , aeFinalize     :: IORef (IO ())
      , aeBodyLimit    :: IORef Int64
      , aeBodyCounter  :: IORef Int64
      , aeLanguages    :: IORef [Text]
      }


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

    bracket_ (return ()) (finalize env) do
      _   <- run env

      st  <- readIORef $ env & aeRespStatus
      hs  <- readIORef $ env & aeRespHeaders
      mk  <- readIORef $ env & aeRespMaker

      resp (mk st hs)

    where
      finalize :: ActionEnv -> IO ()
      finalize = join . readIORef . aeFinalize


  -- |
  -- Type of the function that, given status and headers, completes the
  -- 'Response' by producing a body.
  --
  type ResponseMaker = Status -> ResponseHeaders -> Response


  -- |
  -- Types of the request body.
  --
  data RequestBody
    = BodyUnparsed
      -- ^ Body has not yet been touched.
    | BodyTainted
      -- ^ Body has been partially consumed.
    | BodyForm ([(Text, Text)], [(Text, FileInfo FilePath)])
      -- ^ Body has been successfully parsed as a form.
    | BodyJSON Value
      -- ^ Body has been successfully parsed as a JSON.
    | BodyBytes LBS.ByteString
      -- ^ Body has been successfully read in raw.


  -- |
  -- Create the initial action environment from the 'Request'.
  --
  makeActionEnv :: Request -> IO ActionEnv
  makeActionEnv req = ActionEnv <$> pure req
                                <*> newIORef BodyUnparsed
                                <*> newIORef status200
                                <*> newIORef []
                                <*> newIORef (\st hs -> responseLBS st hs "")
                                <*> newIORef (return ())
                                <*> newIORef (10 * 1024 * 1024)
                                <*> newIORef 0
                                <*> newIORef []


  -- Inspecting Request ------------------------------------------------------


  -- |
  -- Obtain the original 'Request'.
  --
  getRequest :: (MonadAction m) => m Request
  getRequest = aeRequest <$> getActionEnv


  -- |
  -- Obtain the request method, such as \"GET\" or \"POST\".
  --
  getMethod :: (MonadAction m) => m Method
  getMethod = requestMethod <$> getRequest


  -- |
  -- Obtain the request headers.
  --
  getHeaders :: (MonadAction m) => m RequestHeaders
  getHeaders = requestHeaders <$> getRequest


  -- |
  -- Obtain a specific request header.
  --
  getHeaderMaybe :: (MonadAction m) => HeaderName -> m (Maybe ByteString)
  getHeaderMaybe n = lookup n <$> getHeaders


  -- |
  -- Obtain a specific request header or the given default value.
  --
  getHeaderDefault :: (MonadAction m)
                   => HeaderName -> ByteString -> m ByteString
  getHeaderDefault n v = fromMaybe v <$> getHeaderMaybe n


  -- |
  -- Obtain the Accept header value or the default value of @\"*/*\"@.
  --
  getAccept :: (MonadAction m) => m [Media]
  getAccept = parseMedia <$> cs . fromMaybe "*/*"
                         <$> getHeaderMaybe hAccept


  -- |
  -- Obtain the Accept-Charset header value or the default value of @\"*\"@.
  --
  getAcceptCharset :: (MonadAction m) => m [Media]
  getAcceptCharset = parseMedia <$> cs . fromMaybe "*"
                                <$> getHeaderMaybe hAcceptCharset


  -- |
  -- Obtain the Accept-Encoding header value or the default
  -- value of @\"identity,*;q=0\"@.
  --
  getAcceptEncoding :: (MonadAction m) => m [Media]
  getAcceptEncoding = parseMedia <$> cs . fromMaybe "identity,*;q=0"
                                 <$> getHeaderMaybe hAcceptEncoding


  -- |
  -- Obtain the Accept-Language header value or the default value of @\"*\"@.
  --
  getAcceptLanguage :: (MonadAction m) => m [Media]
  getAcceptLanguage = parseMedia <$> cs . fromMaybe "*/*"
                                 <$> getHeaderMaybe hAcceptLanguage


  -- |
  -- Obtain the Content-Type header value or the default value of
  -- @\"application/octet-stream\"@ (true, but meaningless).
  --
  getContentType :: (MonadAction m) => m Media
  getContentType = do
    media <- fmap parseMedia <$> fmap cs <$> getHeaderMaybe hContentType

    case media of
      Just (x:_) -> return x
      _else      -> return "application/octet-stream"


  -- |
  -- Obtain request path split on forward slashes.
  --
  getPathInfo :: (MonadAction m) => m [Text]
  getPathInfo = pathInfo <$> getRequest


  -- |
  -- Obtain request path as an unsplit 'Text'.
  --
  getPathInfoRaw :: (MonadAction m) => m Text
  getPathInfoRaw = cs <$> rawPathInfo <$> getRequest


  -- |
  -- Obtain all request query string parameters.
  --
  getParams :: (MonadAction m) => m [(Text, Text)]
  getParams = map convert <$> queryString <$> getRequest
    where convert (n, v) = (cs n, fromMaybe "" $ cs <$> v)


  -- |
  -- Obtain a specific request query string parameter and parse it
  -- on the fly to the target type. Parsing failure maps to 'Nothing'.
  --
  getParamMaybe :: (MonadAction m, FromParam a) => Text -> m (Maybe a)
  getParamMaybe n = lookup n <$> getParams
                    >>= \case Nothing  -> return $ Nothing
                              Just val -> return $ fromParam val


  -- |
  -- Similar to 'getParamMaybe', but return either the parsed parameter
  -- or the specified default value.
  --
  getParamDefault :: (MonadAction m, FromParam a) => Text -> a -> m a
  getParamDefault n v = fromMaybe v <$> getParamMaybe n


  -- |
  -- Obtain a group of request query string parameters with the same name
  -- and parse them on the fly to the target type.
  --
  getParamList :: (MonadAction m, FromParam a) => Text -> m [a]
  getParamList n = mapMaybe (fromParam . snd)
                   <$> filter ((n ==) . fst)
                   <$> getParams


  -- |
  -- Obtain all request cookies.
  --
  getCookies :: (MonadAction m) => m [(Text, Text)]
  getCookies = do
    mc <- getHeaderMaybe hCookie

    case mc of
      Nothing -> return []
      Just bs -> return $ map cs2 $ parseCookies bs


  -- |
  -- Obtain a specific cookie and parse it on the fly to the target type.
  -- Parsing failure maps to 'Nothing'.
  --
  getCookieMaybe :: (MonadAction m, FromParam a) => Text -> m (Maybe a)
  getCookieMaybe n = lookup n <$> getCookies
                     >>= \case Nothing  -> return $ Nothing
                               Just val -> return $ fromParam val


  -- |
  -- Similar to 'getCookieMaybe', but return either the parsed cookie
  -- or the specified default value.
  --
  getCookieDefault :: (MonadAction m, FromParam a) => Text -> a -> m a
  getCookieDefault n v = fromMaybe v <$> getCookieMaybe n


  -- |
  -- Returns HTTP @Referrer@ header or just @/@.
  --
  -- Useful for redirects back to where the user came from.
  --
  getReferrer :: (MonadAction m) => m Text
  getReferrer = do
    header <- getHeaderDefault hReferer "/"
    return (cs header)


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
  -- more (up to 32kb) than the limit set here where the request
  -- length is not known beforehand.
  --
  setBodyLimit :: (MonadAction m) => Int64 -> m ()
  setBodyLimit = setActionField aeBodyLimit


  -- |
  -- Return the payload size limit set by 'setBodyLimit'.
  --
  getBodyLimit :: (MonadAction m) => m Int64
  getBodyLimit = getActionField aeBodyLimit


  -- |
  -- Read next chunk of the body.
  --
  -- Returns 'Data.ByteString.empty' once the whole body has been consumed.
  --
  -- * Throws 'PayloadTooLarge' if reading next chunk would exceed
  --   the allotted request body limit. See 'setBodyLimit' for more.
  --
  getBodyChunk :: (MonadAction m) => m ByteString
  getBodyChunk = do
    getChunk <- getBodyChunkIO
    liftIO getChunk


  -- |
  -- Return an IO action that will read next chunk of the body.
  --
  -- Returns 'Data.ByteString.empty' once the whole body has been consumed.
  --
  -- * Throws 'PayloadTooLarge' if reading next chunk would exceed
  --   the allotted request body limit. See 'setBodyLimit' for more.
  --
  getBodyChunkIO :: (MonadAction m) => m (IO ByteString)
  getBodyChunkIO = do
    limit    <- getActionField aeBodyLimit
    counter  <- aeBodyCounter <$> getActionEnv
    getChunk <- getRequestBodyChunk <$> getRequest

    return do
      haveRead <- readIORef counter

      if haveRead < limit
         then do
           chunk <- getChunk
           writeIORef counter $ haveRead + fromIntegral (length chunk)
           return chunk

         else do
           throwLimitIO limit

    where
      throwLimitIO :: Int64 -> IO a
      throwLimitIO n = throwIO (PayloadTooLarge, cs msg :: Text)
        where msg = "Limit is " <> show n <> " bytes."


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
  -- * Reading the body can throw 'PayloadTooLarge'.
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
  -- * Throws 'UnsupportedMediaType' if the Content-Type does not
  --   indicate a JSON payload.
  --
  -- * Throws 'BadRequest' if the payload fails to parse.
  --
  -- * Throws 'PayloadTooLarge' if the payload size limit is exceeded.
  --   Use 'setBodyLimit' to adjust the limit to your liking.
  --
  -- * Throws 'InternalError' is the body has already been consumed
  --   and was not cached as JSON.
  --
  getJSON :: (MonadAction m) => m Value
  getJSON = do
    -- First check out our stash.
    cache <- getActionField aeBody

    case cache of
      -- This is ideal, we already have what we need.
      BodyJSON value -> return value

      -- Body has not been parsed yet. This is very good.
      BodyUnparsed -> do
        ctype <- getContentType

        if matchMediaList ctype [ "application/json", "text/json" ]
           then return ()
           else throwError UnsupportedMediaType "Send some JSON!"

        -- Taint and read.
        setActionField aeBody BodyTainted
        body <- getBodyRaw

        -- Try to parse.
        value <- case eitherDecode' body of
                   Left reason -> throwError BadRequest (cs reason)
                   Right value -> return value

        -- Cache and return.
        setActionField aeBody (BodyJSON value)
        return value

      -- Now this is bad. We have already read the body,
      -- but not as a JSON. This is an internal error.
      _else -> do
        throwError InternalError "Body has been parsed as a non-JSON."


  -- |
  -- Read, parse, cache and return form fields sent by the user.
  --
  -- If there were some files uploaded through the form as well,
  -- uploades them to a temporary location and caches information
  -- about them so that 'getFiles' can return them separately.
  --
  -- * Throws 'UnsupportedMediaType' if the Content-Type does not
  --   indicate a form payload.
  --
  -- * Throws 'BadRequest' if the payload fails to parse.
  --
  -- * Throws 'PayloadTooLarge' if the payload size limit is exceeded.
  --   Use 'setBodyLimit' to adjust the limit to your liking.
  --
  getFields :: (MonadAction m) => m [(Text, Text)]
  getFields = map cs2 <$> fst <$> getForm


  -- |
  -- Obtain a specific form field and parse it on the fly to the target type.
  -- Parsing failure maps to 'Nothing'.
  --
  getFieldMaybe :: (MonadAction m, FromParam a) => Text -> m (Maybe a)
  getFieldMaybe n = lookup n <$> getFields
                    >>= \case Nothing  -> return $ Nothing
                              Just val -> return $ fromParam val


  -- |
  -- Similar to 'getFieldMaybe', but return either the parsed field
  -- or the specified default value.
  --
  getFieldDefault :: (MonadAction m, FromParam a) => Text -> a -> m a
  getFieldDefault n v = fromMaybe v <$> getFieldMaybe n


  -- |
  -- Obtain a group of form fields with the same name and parse them on the
  -- fly to the target type.
  --
  getFieldList :: (MonadAction m, FromParam a) => Text -> m [a]
  getFieldList n = mapMaybe (fromParam . snd)
                   <$> filter ((n ==) . fst)
                   <$> getFields


  -- |
  -- Identical to 'getFields', except it returns information about
  -- files uploaded through the form.
  --
  getFiles :: (MonadAction m) => m [(Text, FileInfo FilePath)]
  getFiles = snd <$> getForm


  -- |
  -- Obtain a specific form file
  --
  getFileMaybe :: (MonadAction m) => Text -> m (Maybe (FileInfo FilePath))
  getFileMaybe n = lookup n <$> getFiles
                    >>= \case Nothing  -> return $ Nothing
                              Just val -> return $ Just val


  -- |
  -- Obtain a group of form files with the same name.
  --
  getFileList :: (MonadAction m) => Text -> m [FileInfo FilePath]
  getFileList n = map snd . filter ((n ==) . fst) <$> getFiles


  -- |
  -- Backend for both 'getFields' and 'getFiles' that parses,
  -- caches and returns form data.
  --
  getForm :: (MonadAction m)
          => m ([(Text, Text)], [(Text, FileInfo FilePath)])
  getForm = do
    cache <- getActionField aeBody

    case cache of
      -- This is ideal, we already have what we need.
      BodyForm form -> return form

      -- Body has not been parsed yet. This is very good.
      BodyUnparsed -> do
        bodyType <- getRequestBodyType <$> getRequest
        getChunk <- getBodyChunkIO

        case bodyType of
          Nothing -> throwError UnsupportedMediaType "Send some form!"
          Just bt -> do
            -- Prepare for uploaded files finalization.
            rtis <- createInternalState
            registerFinalizer (closeInternalState rtis)

            -- Parse the form data.
            form' <- liftIO do
              sinkRequestBody (tempFileBackEnd rtis) bt getChunk

            -- Convert ByteString to Text fields.
            let form = csForm form'

            -- Cache and return.
            setActionField aeBody (BodyForm form)
            return form

      -- Now this is bad. We have already read the body,
      -- but not as a form. This is an internal error.
      _else -> do
        throwError InternalError "Body has been parsed as a non-form."


  -- |
  -- Convert form names and fields from 'ByteString' to 'Text'.
  --
  csForm :: ([Param], [File FilePath])
         -> ([(Text, Text)], [(Text, FileInfo FilePath)])
  csForm (ps, fs) = (map cs2 ps, map cs1 fs)


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
    cache <- getActionField aeBody

    case cache of
      -- This is ideal, we already have what we need.
      BodyBytes bstr -> return bstr

      -- Body has not been parsed yet. This is very good.
      BodyUnparsed -> do
        -- Taint and read.
        setActionField aeBody BodyTainted
        body <- getBodyRaw

        -- Force it whole.
        _len  <- LBS.length <$> pure body

        -- Cache and return.
        setActionField aeBody (BodyBytes body)
        return body

      -- Now this is bad. We have already read the body,
      -- but not as a raw data. This is an internal error.
      _else -> do
        throwError InternalError "Body has already been parsed."


  -- Building Response -------------------------------------------------------


  -- |
  -- Set the status to use when building our 'Response'.
  --
  setStatus :: (MonadAction m) => Status -> m ()
  setStatus = setActionField aeRespStatus


  -- |
  -- Set headers to use when building our 'Response'.
  --
  setHeaders :: (MonadAction m) => ResponseHeaders -> m ()
  setHeaders = setActionField aeRespHeaders


  -- |
  -- Append a single 'Response' header without checking.
  --
  addHeader :: (MonadAction m) => HeaderName -> ByteString -> m ()
  addHeader n v = modifyActionField aeRespHeaders ((n, v) :)


  -- |
  -- Set a single 'Response' header to a new value.
  -- If the header has been given multiple times, leave only one.
  --
  setHeader :: (MonadAction m) => HeaderName -> ByteString -> m ()
  setHeader n v = modifyActionField aeRespHeaders modify
    where
      modify hs = (n, v) : deleteBy headerEq (n, v) hs


  -- |
  -- Set header only if it has not been set yet.
  --
  -- Used by the 'sendHTML', 'sendJSON' and other similar functions.
  --
  defaultHeader :: (MonadAction m) => HeaderName -> ByteString -> m ()
  defaultHeader n v = modifyHeader n (fromMaybe v)


  -- |
  -- Replace a single 'Response' header with a new one that is constructed
  -- by applying the supplied function to the value of the previous one. Only
  -- the last set header is modified, other matching headers are discarded.
  --
  -- Used in conjunction with 'maybe' this can be used to append header
  -- values in a sensible way.
  --
  -- @
  -- modifyHeader "Vary" $ maybe "Accept" (<> ", Accept")
  -- @
  --
  modifyHeader :: (MonadAction m)
               => HeaderName -> (Maybe ByteString -> ByteString) -> m ()
  modifyHeader n fn = modifyActionField aeRespHeaders modify
    where
      modify hs = (n, v') : deleteBy headerEq (n, v') hs
        where v' = fn (lookup n hs)


  -- |
  -- Set a cookie with just a name and a value.
  --
  -- Such cookies are valid for the whole domain, expire when the browser
  -- is closed, can be accessed from JavaScript and may be sent with
  -- cross-site requests.
  --
  -- Do not use cookies set in this way for anything else than storing
  -- simple user preferences.
  --
  setCookie :: (MonadAction m) => Text -> Text -> m ()
  setCookie name value = do
    setCookieEx $ defaultSetCookie { setCookieName  = cs name
                                   , setCookieValue = cs value
                                   , setCookiePath  = Just "/"
                                   }


  -- |
  -- Set a cookie using the 'Web.Cookie.SetCookie' directly.
  --
  setCookieEx :: (MonadAction m) => SetCookie -> m ()
  setCookieEx cookie = do
    addHeader hSetCookie $ cs $ toLazyByteString $ renderSetCookie cookie


  -- |
  -- Default @Content-Type@ to @text/html; charset=utf8@
  -- and set the response body to the provided byte string.
  --
  sendHTML :: (MonadAction m) => HtmlT m a -> m ()
  sendHTML html = do
    defaultHeader hContentType "text/html; charset=utf8"
    builder <- execHtmlT html
    setResponseBS (toLazyByteString builder)


  -- |
  -- Default @Content-Type@ to @text/plain; charset=utf8@
  -- and set the response body to the provided text.
  --
  sendText :: (MonadAction m) => Text -> m ()
  sendText text = do
    defaultHeader hContentType "text/plain; charset=utf8"
    setResponseText' text


  -- |
  -- Default @Content-Type@ to @text/plain; charset=utf8@
  -- and set the response body to the provided string.
  --
  sendString :: (MonadAction m) => String -> m ()
  sendString str = do
    defaultHeader hContentType "text/plain; charset=utf8"
    setResponseString str


  -- |
  -- Default @Content-Type@ to @application/json@ and set the response
  -- body to the result of encoding provided Aeson value.
  --
  sendJSON :: (MonadAction m, ToJSON a) => a -> m ()
  sendJSON json = do
    defaultHeader hContentType "application/json"
    setResponseBS (encode json)


  -- |
  -- Set the @Location@ header and response status to redirect the user
  -- elsewhere.
  --
  redirect :: (MonadAction m) => Text -> m ()
  redirect location = do
    setStatus status302
    setHeader hLocation (cs location)


  -- |
  -- Redirect the user to where he came from using 'getReferrer'.
  --
  redirectBack :: (MonadAction m) => m ()
  redirectBack = redirect =<< getReferrer


  -- |
  -- Create response body using a file.
  --
  -- Optional 'FilePath' argument allows for Range header support.
  --
  setResponseFile :: (MonadAction m) => FilePath -> Maybe FilePart -> m ()
  setResponseFile fp mfp = do
    setActionField aeRespMaker \st hs -> responseFile st hs fp mfp


  -- |
  -- Create response body using a 'Builder'.
  --
  setResponseBuilder :: (MonadAction m) => Builder -> m ()
  setResponseBuilder bld = do
    setActionField aeRespMaker \st hs -> responseBuilder st hs bld


  -- |
  -- Create response body using a lazy 'LBS.ByteString'.
  --
  setResponseBS :: (MonadAction m) => LBS.ByteString -> m ()
  setResponseBS bs = do
    setActionField aeRespMaker \st hs -> responseLBS st hs bs


  -- |
  -- Create response body using a strict 'ByteString'.
  --
  setResponseBS' :: (MonadAction m) => ByteString -> m ()
  setResponseBS' = setResponseBS . cs


  -- |
  -- Create response body using a lazy 'LT.Text'.
  --
  setResponseText :: (MonadAction m) => LT.Text -> m ()
  setResponseText = setResponseBS . cs


  -- |
  -- Create response body using a strict 'Text'.
  --
  setResponseText' :: (MonadAction m) => Text -> m ()
  setResponseText' = setResponseBS . cs


  -- |
  -- Create response body using a 'String'.
  --
  setResponseString :: (MonadAction m) => String -> m ()
  setResponseString = setResponseBS . cs


  -- |
  -- Create response body using a stream of values.
  --
  setResponseStream :: (MonadAction m) => StreamingBody -> m ()
  setResponseStream strm = do
    setActionField aeRespMaker \st hs -> responseStream st hs strm


  -- |
  -- Escape the established narrative of 'Response' bodies and take over
  -- the connection for any purpose you deep practical. Ideal for WebSockets
  -- and such.
  --
  -- The secondary 'Response' is used when upgrading is not supported by
  -- the underlying web server technology.
  --
  -- NOTE: Ignores both status and headers.
  --
  -- NOTE: Try not to read from the body before starting the raw response
  --       or risk encountering an undefined behavior.
  --
  setResponseRaw :: (MonadAction m)
                 => (IO ByteString -> (ByteString -> IO ()) -> IO ())
                 -> Response
                 -> m ()
  setResponseRaw comm resp = do
    setActionField aeRespMaker \_st _hs -> responseRaw comm resp


  -- |
  -- Same as 'throwError', but with a message.
  --
  throwError :: (MonadAction m) => RequestError -> Text -> m a
  throwError exn msg = liftIO $ throwIO (exn, msg)


  -- Localization ------------------------------------------------------------


  -- |
  -- Get list of languages in order of their preference to be used
  -- when localizing the result of the action.
  --
  -- Languages must be set using the 'setLanguages' function or through
  -- the localization tools found in the "Web.Hikaru.Locale" module.
  --
  getLanguages :: (MonadAction m) => m [Text]
  getLanguages = getActionField aeLanguages


  -- |
  -- Set list of localization languages.
  --
  -- See 'getLanguages' above for more information.
  --
  setLanguages :: (MonadAction m) => [Text] -> m ()
  setLanguages = setActionField aeLanguages


  -- Finalizing --------------------------------------------------------------


  -- |
  -- Register an IO action to run once the request is either handler
  -- or fails with an error.
  --
  registerFinalizer :: (MonadAction m) => IO a -> m ()
  registerFinalizer fin = do
    modifyActionField aeFinalize (fin >>)


  -- Misc Utilities ----------------------------------------------------------


  -- |
  -- Helper to compare two headers by their name.
  --
  headerEq :: (Eq a) => (a, b) -> (a, b) -> Bool
  headerEq (x, _) (y, _) = x == y


  -- |
  -- Helper to apply 'cs' to both elements of a 2-tuple.
  --
  cs2 :: ( ConvertibleStrings a c
         , ConvertibleStrings b d
         )
      => (a, b) -> (c, d)
  cs2 (x, y) = (cs x, cs y)


  -- |
  -- Helper to apply 'cs' to the first element of a 2-tuple.
  --
  cs1 :: (ConvertibleStrings a c) => (a, b) -> (c, b)
  cs1 (x, y) = (cs x, y)


-- vim:set ft=haskell sw=2 ts=2 et:
