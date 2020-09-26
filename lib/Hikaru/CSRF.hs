{-|
Module      :  Hikaru.CSRF
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides CSRF mitigation utilities.
-}

module Hikaru.CSRF
  ( generateToken
  , isTokenValid
  )
where
  import Relude

  import Crypto.Hash
  import Crypto.MAC.HMAC
  import Data.String.Conversions
  import Data.Text (splitOn)
  import Data.Time.Clock.POSIX (getPOSIXTime)

  import Hikaru.Action


  -- |
  -- Generate an anti-CSRF token to be used with forms.
  --
  generateToken :: (MonadAction m) => m Text
  generateToken = do
    now    <- getTimestamp
    secret <- getConfigDefault "csrf.secret" ""

    let signature = sign now secret
     in return $ mconcat [ show now, ":", signature ]


  -- |
  -- Verify that the anti-CSRF token is currently valid.
  --
  isTokenValid :: (MonadAction m) => Text -> m Bool
  isTokenValid token = do
    case splitOn ":" token of
      [time, signature] -> do
        case readMaybe (cs time) of
          Just (timestamp :: Int64) -> do
            now    <- getTimestamp
            valid  <- getConfigDefault "csrf.validity" 86400
            secret <- getConfigDefault "csrf.secret" ""

            if timestamp + valid >= now
               then return (sign timestamp secret == signature)
               else return False

          Nothing -> return False

      _else -> return False


  -- Internals --------------------------------------------------------------


  getTimestamp :: (MonadIO m) => m Int64
  getTimestamp = round <$> liftIO getPOSIXTime


  sign :: Int64 -> Text -> Text
  sign timestamp secret = show $ hmacGetDigest digest
    where
      digest      = hmac timeBytes secretBytes :: HMAC SHA256
      secretBytes = cs secret :: ByteString
      timeBytes   = show timestamp :: ByteString


-- vim:set ft=haskell sw=2 ts=2 et:
