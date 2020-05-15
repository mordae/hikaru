{-|
Module      :  Hikaru.FormSpec
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)
-}

module Hikaru.FormSpec
  ( spec
  )
where
  import BasePrelude

  import Hikaru ()
  import Hikaru.Test


  -- Spec --------------------------------------------------------------------


  spec :: Spec
  spec = do
    describe "form" do
      it "has tests written" do
        False


-- vim:set ft=haskell sw=2 ts=2 et:
