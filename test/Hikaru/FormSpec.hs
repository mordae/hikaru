-- |
-- Module      :  Hikaru.FormSpec
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Hikaru.FormSpec
  ( spec
  )
where
  import Praha

  import Hikaru ()
  import Hikaru.Test


  -- Spec --------------------------------------------------------------------


  spec :: Spec
  spec = do
    describe "form" do
      return ()


-- vim:set ft=haskell sw=2 ts=2 et:
