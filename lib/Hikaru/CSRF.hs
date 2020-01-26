{-|
Module      :  Hikaru.CSRF
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

CSRF mitigation utilities.
-}

module Hikaru.CSRF
  ( MonadCsrf(..)
  , generateToken
  , isTokenValid
  )
where
  import BasePrelude

  import Control.Monad.Trans
  import Crypto.Hash
  import Crypto.MAC.HMAC
  import Data.ByteString (ByteString)
  import Data.String.Conversions
  import Data.Text (Text, splitOn)
  import Data.Time.Clock.POSIX (getPOSIXTime)


  class (MonadIO m) => MonadCsrf m where
    csrfTokenValidity :: m Int64
    csrfTokenSecret   :: m Text

    default csrfTokenValidity
      :: (MonadTrans t, MonadCsrf n, m ~ t n) => m Int64
    csrfTokenValidity = lift csrfTokenValidity

    default csrfTokenSecret
      :: (MonadTrans t, MonadCsrf n, m ~ t n) => m Text
    csrfTokenSecret = lift csrfTokenSecret


  -- |
  -- TODO
  --
  generateToken :: (MonadCsrf m) => m Text
  generateToken = do
    now    <- getTimestamp
    secret <- csrfTokenSecret

    let signature = sign now secret
     in return $ mconcat [ cs (show now), ":", signature ]


  -- |
  -- TODO
  --
  isTokenValid :: (MonadCsrf m) => Text -> m Bool
  isTokenValid token = do
    case splitOn ":" token of
      [time, signature] -> do
        case readMaybe (cs time) of
          Just (timestamp :: Int64) -> do
            now    <- getTimestamp
            valid  <- csrfTokenValidity
            secret <- csrfTokenSecret

            if timestamp + valid >= now
               then return (sign timestamp secret == signature)
               else return False

          Nothing -> return False

      _else -> return False


  -- Internals --------------------------------------------------------------


  getTimestamp :: (MonadIO m) => m Int64
  getTimestamp = round <$> liftIO getPOSIXTime


  sign :: Int64 -> Text -> Text
  sign timestamp secret = cs $ show $ hmacGetDigest digest
    where
      digest      = hmac timeBytes secretBytes :: HMAC SHA256
      secretBytes = cs secret :: ByteString
      timeBytes   = cs time :: ByteString
      time        = show timestamp


-- vim:set ft=haskell sw=2 ts=2 et:
