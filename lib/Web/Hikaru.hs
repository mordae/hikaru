{-|
Module      :  Web.Hikaru
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module re-exports other modules in the package.
-}

module Web.Hikaru
  (
  -- * Exported Modules
    module Web.Hikaru.Action
  , module Web.Hikaru.Dispatch
  , module Web.Hikaru.Form
  , module Web.Hikaru.Link
  , module Web.Hikaru.Localize
  , module Web.Hikaru.Media
  , module Web.Hikaru.Route
  , module Web.Hikaru.Types
  )
where
  import Web.Hikaru.Action
  import Web.Hikaru.Dispatch
  import Web.Hikaru.Form
  import Web.Hikaru.Link
  import Web.Hikaru.Localize
  import Web.Hikaru.Media
  import Web.Hikaru.Route
  import Web.Hikaru.Types


-- vim:set ft=haskell sw=2 ts=2 et:
