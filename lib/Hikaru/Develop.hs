{-|
Module      :  Hikaru.Develop
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides development and testing utilities.
-}

module Hikaru.Develop
  ( developWith
  )
where
  import Relude

  import Control.Concurrent (killThread, forkFinally)
  import Foreign.Store


  -- |
  -- To be used with @ghcid@ to reload the app when the source changes.
  --
  developWith :: IO () -> IO ()
  developWith main = do
    store <- mapM readStore =<< lookupStore 1
    stash <- case store of
               Nothing -> do
                 stash <- newEmptyMVar
                 writeStore (Store 1) stash
                 return stash

               Just stash -> do
                 return stash

    tryTakeMVar stash >>= \case
      Nothing          -> return ()
      Just (lock, tid) -> killThread tid >> takeMVar lock

    lock' <- newEmptyMVar
    tid'  <- forkFinally main $ const $ putMVar lock' ()
    putMVar stash (lock', tid')


-- vim:set ft=haskell sw=2 ts=2 et:
