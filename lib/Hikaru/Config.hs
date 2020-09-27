{-|
Module      :  Hikaru.Config
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides means to read configuration from environment and files.

Example:

@
  base <- 'configDefault'
  file <- 'configFromFile' \"site.env\"
  env  <- 'configFromEnv'

  let cfg = env <> file <> base
@
-}

module Hikaru.Config
  ( Config
  , configFromEnv
  , configFromFile
  , configDefault

  -- * Secrets
  , generateSecret
  )
where
  import Relude hiding (lines)

  import qualified Data.Map as Map

  import Crypto.Random.Entropy
  import Data.ByteArray.Encoding
  import Data.List (lines, span)
  import Data.String.Conversions
  import System.Environment


  -- |
  -- Website configuration to be passed to the action.
  --
  type Config = Map.Map Text Text


  -- |
  -- Read configuration from program environment.
  --
  configFromEnv :: (MonadIO m) => m Config
  configFromEnv = Map.fromList <$> map conv <$> liftIO getEnvironment
    where conv (k, v) = (cs k, cs v)


  -- |
  -- Read configuration from file.
  --
  -- To read configuration from a file and then update it from the environment
  -- use "<>" like this:
  --
  -- @
  -- def <- 'configFromFile' \"site.env\"
  -- env <- 'configFromEnv'
  --
  -- let cfg = env <> def
  -- @
  --
  configFromFile :: (MonadIO m) => FilePath -> m Config
  configFromFile path = Map.fromList <$> map parse <$> lines <$> readFile path
    where
      parse       = conv . span (/= '=')
      conv (k, v) = (cs k, cs $ drop 1 v)


  -- |
  -- Generate default configuration with keys required by Hikaru.
  --
  -- This includes @CSRF_SECRET@ which should be set to something persistent,
  -- but if it's not, it must be set to a random value upon startup at least.
  --
  configDefault :: (MonadIO m) => m Config
  configDefault = do
    secret <- generateSecret 16

    return $ Map.fromList [ ("CSRF_SECRET",   secret)
                          , ("CSRF_VALIDITY", "86400")
                          ]


  -- |
  -- Generate a random base64-encoded secret of given length
  -- (in decoded bytes).
  --
  generateSecret :: (MonadIO m) => Int -> m Text
  generateSecret n = do
    (bstr :: ByteString) <- liftIO $ getEntropy n

    let (bstr64 :: ByteString) = convertToBase Base64 bstr
     in return (cs bstr64)


-- vim:set ft=haskell sw=2 ts=2 et:
