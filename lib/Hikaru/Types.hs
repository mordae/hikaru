{-|
Module      :  Hikaru.Types
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3.0-or-later

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides types common for multiple other modules.
-}

module Hikaru.Types
  ( Param(..)
  , RequestError(..)
  , Severity(..)
  , defaultHandler
  )
where
  import Praha

  import Data.ByteString (ByteString)
  import Data.Text (pack, unpack)
  import Network.HTTP.Types.Header
  import Network.HTTP.Types.Status
  import Network.Wai

  import qualified Data.ByteString.Lazy
  import qualified Data.Text.Encoding
  import qualified Data.Text.Encoding.Error
  import qualified Data.Text.Lazy


  -- |
  -- Types that can be parsed from a route segment or a query string
  -- parameter into some kind of value. One does not usually pass around
  -- more complex arguments than these, so forgive the limited menu.
  --
  class Param a where
    fromParam :: Text -> Maybe a
    toParam   :: a -> Text

  instance (Param a) => Param (Maybe a) where
    fromParam = Just . fromParam
    {-# INLINE fromParam #-}

    toParam = maybe "" toParam
    {-# INLINE toParam #-}

  instance Param Int where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Int8 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Int16 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Int32 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Int64 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Word where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Word8 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Word16 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Word32 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Word64 where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Integer where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Natural where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Float where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param Double where
    fromParam = readMaybe . unpack
    {-# INLINE fromParam #-}

    toParam = pack . show
    {-# INLINE toParam #-}

  instance Param () where
    fromParam _ = Just ()
    {-# INLINE fromParam #-}

    toParam _ = ""
    {-# INLINE toParam #-}

  instance Param Bool where
    fromParam "true"  = Just True
    fromParam "True"  = Just True
    fromParam "yes"   = Just True
    fromParam "Yes"   = Just True
    fromParam "on"    = Just True
    fromParam "On"    = Just True
    fromParam "1"     = Just True
    fromParam "false" = Just False
    fromParam "False" = Just False
    fromParam "off"   = Just False
    fromParam "Off"   = Just False
    fromParam "no"    = Just False
    fromParam "No"    = Just False
    fromParam "0"     = Just False
    fromParam _else   = Nothing
    {-# INLINE fromParam #-}

    toParam True  = "true"
    toParam False = "false"
    {-# INLINE toParam #-}

  instance Param Char where
    fromParam inp = case (unpack inp) of
                      [x]   -> Just x
                      _else -> Nothing
    {-# INLINE fromParam #-}

    toParam char = pack [char]
    {-# INLINE toParam #-}

  instance Param String where
    fromParam "" = Nothing
    fromParam sp = Just (cs sp)
    {-# INLINE fromParam #-}

    toParam = cs
    {-# INLINE toParam #-}

  instance Param Text where
    fromParam "" = Nothing
    fromParam sp = Just sp
    {-# INLINE fromParam #-}

    toParam = id
    {-# INLINE toParam #-}

  instance Param Data.Text.Lazy.Text where
    fromParam "" = Nothing
    fromParam sp = Just $ Data.Text.Lazy.fromStrict sp
    {-# INLINE fromParam #-}

    toParam = Data.Text.Lazy.toStrict
    {-# INLINE toParam #-}

  instance Param Data.ByteString.ByteString where
    fromParam "" = Nothing
    fromParam sp = Just $ Data.Text.Encoding.encodeUtf8 sp
    {-# INLINE fromParam #-}

    toParam = Data.Text.Encoding.decodeUtf8With
                Data.Text.Encoding.Error.lenientDecode
    {-# INLINE toParam #-}

  instance Param Data.ByteString.Lazy.ByteString where
    fromParam "" = Nothing
    fromParam sp = Just $ Data.ByteString.Lazy.fromStrict
                        $ Data.Text.Encoding.encodeUtf8 sp
    {-# INLINE fromParam #-}

    toParam = Data.Text.Encoding.decodeUtf8With
                Data.Text.Encoding.Error.lenientDecode
            . Data.ByteString.Lazy.toStrict
    {-# INLINE toParam #-}


  -- |
  -- Errors used both by "Hikaru.Action" and "Hikaru.Route"
  -- to report problems with the requests sent by the user.
  --
  -- Order by the severity from the least to the most severe.
  --
  data RequestError
    = TooManyRequests
      -- ^ The user has sent too many requests in a given amount of time.
    | PreconditionRequired
      -- ^ Server requires the request to be conditional.
    | Unprocessable
      -- ^ The request was well-formed but had some semantic errors.
    | ExpectationFailed
      -- ^ Expectation indicated by the Expect header can't be met.
    | RangeNotSatisfiable
      -- ^ The range specified by the Range header can't be fulfilled.
    | UnsupportedMediaType
      -- ^ Request payload format is not supported by the server.
    | PayloadTooLarge
      -- ^ Request entity is larger than limits defined by server.
    | PreconditionFailed
      -- ^ The client has indicated preconditions in its headers which
      --   the server does not meet.
    | LengthRequired
      -- ^ Content-Length header field is mandatory but missing.
    | Gone
      -- ^ Requested content has been permanently deleted from server,
      --   with no forwarding address.
    | Conflict
      -- ^ Request conflicts with the current state of the server.
    | NotAcceptable
      -- ^ After performing content negotiation, no content following
      --   the criteria given by the user agent has remained.
    | MethodNotAllowed
      -- ^ The request method is not available for the resource.
    | NotFound
      -- ^ Requested resource could not be found.
    | Forbidden
      -- ^ The client does not have access rights to the content.
      --   Unlike 'Unauthorized', the client's identity is known to
      --   the server.
    | Unauthorized
      -- ^ Client must authenticate itself to get the requested response.
    | BadRequest
      -- ^ The request was not well-formed.
    | ServiceUnavailable
      -- ^ The server is not ready to handle the request.
    | InternalError
      -- ^ The server has encountered a situation it doesn't know
      --   how to handle.
    deriving (Eq, Ord, Show, Typeable)

  -- |
  -- Adding two errors chooses the less severe one.
  --
  instance Semigroup RequestError where
    (<>) = min
    {-# INLINE (<>) #-}

  -- |
  -- Request errors can be thrown and catched, when accompanied with
  -- a short message.
  --
  instance Exception (RequestError, Text)


  -- |
  -- Default handlers for when user did not register a custom one.
  --
  defaultHandler :: RequestError -> Text -> Application
  defaultHandler meth msg _ resp =
    case meth of
      TooManyRequests      -> response status429 "420 Too Many Requests"
      PreconditionRequired -> response status428 "428 Precondition Required"
      Unprocessable        -> response status422 "422 Unprocessable"
      ExpectationFailed    -> response status417 "417 Expectation Failed"
      RangeNotSatisfiable  -> response status416 "416 Range Not Satisfiable"
      UnsupportedMediaType -> response status415 "415 Unsupported Media Type"
      PayloadTooLarge      -> response status413 "413 Payload Too Large"
      PreconditionFailed   -> response status412 "412 Precondition Failed"
      LengthRequired       -> response status411 "411 Length Required"
      Gone                 -> response status410 "410 Gone"
      Conflict             -> response status409 "409 Conflict"
      NotAcceptable        -> response status406 "406 Not Acceptable"
      Forbidden            -> response status403 "403 Forbidden"
      Unauthorized         -> response status401 "401 Unauthorized"
      BadRequest           -> response status400 "400 Bad Request"
      ServiceUnavailable   -> response status503 "503 Service Unavailable"
      InternalError        -> response status500 "500 Internal Server Error"
      MethodNotAllowed     -> response status405 "405 Method Not Allowed"
      NotFound             -> response status404 "404 Not Found"

    where
      response st = resp . responseLBS st [(hContentType, "text/plain")]
                         . cs . (\str -> if msg == "" then str else msg)


  -- |
  -- Information severity to be used for messages.
  --
  data Severity
    = Success
    | Warning
    | Danger
    deriving (Eq, Ord, Enum, Show)

  -- |
  -- Concatenation yields the higher severity.
  --
  instance Semigroup Severity where
    (<>) = max

  -- |
  -- 'Success' is the neutral element.
  --
  instance Monoid Severity where
    mempty = Success


-- vim:set ft=haskell sw=2 ts=2 et:
