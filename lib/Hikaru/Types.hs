{-|
Module      :  Hikaru.Types
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides types common for multiple other modules.
-}

module Hikaru.Types
  ( FromParam(..)
  , ToParam(..)
  , RequestError(..)
  , defaultHandler
  )
where
  import BasePrelude

  import Data.ByteString (ByteString)
  import Data.String.Conversions
  import Data.Text (Text, pack, unpack)
  import Network.HTTP.Types.Status
  import Network.HTTP.Types.Header
  import Network.Wai

  import qualified Data.ByteString.Lazy
  import qualified Data.Text.Lazy


  -- |
  -- Types that can be parsed from a route segment or a query string
  -- parameter into some kind of value. One does not usually pass around
  -- more complex arguments than these, so forgive the limited menu.
  --
  class FromParam a where
    fromParam :: Text -> Maybe a

  instance FromParam Int where
    fromParam = readMaybe . unpack

  instance FromParam Int8 where
    fromParam = readMaybe . unpack

  instance FromParam Int16 where
    fromParam = readMaybe . unpack

  instance FromParam Int32 where
    fromParam = readMaybe . unpack

  instance FromParam Int64 where
    fromParam = readMaybe . unpack

  instance FromParam Word where
    fromParam = readMaybe . unpack

  instance FromParam Word8 where
    fromParam = readMaybe . unpack

  instance FromParam Word16 where
    fromParam = readMaybe . unpack

  instance FromParam Word32 where
    fromParam = readMaybe . unpack

  instance FromParam Word64 where
    fromParam = readMaybe . unpack

  instance FromParam Integer where
    fromParam = readMaybe . unpack

  instance FromParam Natural where
    fromParam = readMaybe . unpack

  instance FromParam Float where
    fromParam = readMaybe . unpack

  instance FromParam Double where
    fromParam = readMaybe . unpack

  instance FromParam Bool where
    fromParam "true"  = Just True
    fromParam "True"  = Just True
    fromParam "false" = Just False
    fromParam "False" = Just False
    fromParam _else   = Nothing

  instance FromParam Char where
    fromParam inp = case (unpack inp) of
                      [x]   -> Just x
                      _else -> Nothing

  instance FromParam String where
    fromParam = Just . unpack

  instance FromParam Text where
    fromParam = Just . id

  instance FromParam Data.Text.Lazy.Text where
    fromParam = Just . cs

  instance FromParam Data.ByteString.ByteString where
    fromParam = Just . cs

  instance FromParam Data.ByteString.Lazy.ByteString where
    fromParam = Just . cs


  -- |
  -- Values that can be represented as a piece of 'Text' to be used in a
  -- route segment or a query string. One does not usually pass around
  -- more complex arguments than these, so forgive the limited menu.
  --
  class ToParam a where
    toParam :: a -> Text

  instance ToParam Int where
    toParam = pack . show

  instance ToParam Int8 where
    toParam = pack . show

  instance ToParam Int16 where
    toParam = pack . show

  instance ToParam Int32 where
    toParam = pack . show

  instance ToParam Int64 where
    toParam = pack . show

  instance ToParam Word where
    toParam = pack . show

  instance ToParam Word8 where
    toParam = pack . show

  instance ToParam Word16 where
    toParam = pack . show

  instance ToParam Word32 where
    toParam = pack . show

  instance ToParam Word64 where
    toParam = pack . show

  instance ToParam Integer where
    toParam = pack . show

  instance ToParam Natural where
    toParam = pack . show

  instance ToParam Float where
    toParam = pack . show

  instance ToParam Double where
    toParam = pack . show

  instance ToParam Bool where
    toParam True  = "true"
    toParam False = "false"

  instance ToParam Char where
    toParam char = pack [char]

  instance ToParam String where
    toParam = pack

  instance ToParam Text where
    toParam = id

  instance ToParam Data.Text.Lazy.Text where
    toParam = cs

  instance ToParam Data.ByteString.ByteString where
    toParam = cs

  instance ToParam Data.ByteString.Lazy.ByteString where
    toParam = cs


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
                         . cs . (<> "\n" <> msg)


-- vim:set ft=haskell sw=2 ts=2 et:
