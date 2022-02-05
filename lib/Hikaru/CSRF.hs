-- |
-- Module      :  Hikaru.CSRF
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides CSRF mitigation utilities.
--

module Hikaru.CSRF
  ( generateToken
  , isTokenValid
  )
where
  import Praha
  import Praha.Config.Environment

  import Hikaru.Action

  import Data.Text (splitOn)
  import Data.Time.Clock.POSIX (getPOSIXTime)
  import OpenSSL
  import OpenSSL.EVP.Base64
  import OpenSSL.EVP.Digest
  import OpenSSL.Random
  import System.IO.Unsafe


  sha256 :: Digest
  sha256 = case unsafePerformIO (withOpenSSL (getDigestByName "sha256")) of
             Nothing -> error "OpenSSL does not provide sha256"
             Just dg -> dg
  {-# NOINLINE sha256 #-}


  -- |
  -- Generate an anti-CSRF token to be used with forms.
  --
  -- Uses the @HIKARU_SECRET@ configuration key.
  --
  generateToken :: (MonadAction m) => m Text
  generateToken = do
    now    <- getTimestamp
    secret <- getSecret

    let signature = sign secret now
     in return $ mconcat [ tshow now, ":", signature ]


  -- |
  -- Verify that the anti-CSRF token is currently valid.
  --
  -- Uses the @CSRF_SECRET@ and @CSRF_VALIDITY@ configuration keys.
  -- Valididy defaults to 86400 seconds (24 hours).
  --
  isTokenValid :: (MonadAction m) => Text -> m Bool
  isTokenValid token = do
    case splitOn ":" token of
      [time, signature] -> do
        case readMaybe (cs time) of
          Just (timestamp :: Int64) -> do
            now    <- getTimestamp
            valid  <- getConfigDefault "CSRF_VALIDITY" 86400
            secret <- getConfigDefault "CSRF_SECRET" ""

            if timestamp + valid >= now
               then return (sign secret timestamp == signature)
               else return False

          Nothing -> return False

      _else -> return False


  -- Internals --------------------------------------------------------------


  getTimestamp :: (MonadIO m) => m Int64
  getTimestamp = round <$> liftIO getPOSIXTime


  -- |
  -- Use the secret from environment or put there an actual one.
  --
  getSecret :: (MonadIO m) => m Text
  getSecret = do
    maybeSecret <- getConfigMaybe "HIKARU_SECRET"

    case maybeSecret of
      Just secret -> return secret
      Nothing -> do
        secret <- generateSecret 16
        setConfig "HIKARU_SECRET" secret
        return secret


  sign :: Text -> Int64 -> Text
  sign secret timestamp = cs signature
    where
      signature   = hmacBS sha256 secretBytes timeBytes
      secretBytes = cs secret
      timeBytes   = cs (show timestamp)


  -- |
  -- Generate a random base64-encoded secret of given length
  -- (in decoded bytes).
  --
  generateSecret :: (MonadIO m) => Int -> m Text
  generateSecret n = do
    (bstr :: ByteString) <- liftIO $ prandBytes n

    let bstr64 = encodeBase64BS bstr
     in return (cs bstr64)



-- vim:set ft=haskell sw=2 ts=2 et:
