-- |
-- Module      :  Hikaru.Widget
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module tries to simplify building custom HTML widgets that react
-- to query string parameters and render with different CSS frameworks,
-- such as Bootstrap or Tailwind.
--

module Hikaru.Widget
  ( Render(..)
  , RenderWidget(..)
  )
where
  import Crude.Prelude

  import Hikaru.HTML


  -- |
  -- Class to render values using various HTML/CSS frameworks.
  --
  -- Example:
  --
  -- @
  -- -- For Bootstrap 5
  -- data B5 = B5
  --
  -- data Outcome = Success | Failure
  --
  -- instance (Monad m) => 'Render' B5 m Outcome where
  --   'render' Success = span_ [class_ \"alert alert-success\"] \"Success\"
  --   'render' Failure = span_ [class_ \"alert alert-danger\"] \"Failure\"
  --
  -- demo :: HtmlT m ()
  -- demo = do
  --   'render' @B5 Success
  --   'render' @B5 Failure
  -- @
  --
  class (Monad m) => Render t a m where
    render :: a -> HtmlT m ()


  -- |
  -- Class to render abstract widgets using various HTML/CSS frameworks.
  --
  -- Example:
  --
  -- @
  -- -- For Tailwind
  -- data TW = TW
  --
  -- data LoggedInUser = LoggedInUser
  --
  -- instance 'RenderWidget' TW LoggedInUser MyAction where
  --   'renderWidget' = do
  --     UserInfo{userName, userId} <- getUserInfo
  --     a_ [ 'Hikaru.Link.rhref_' getUserDetail [\"id\" '.=' tshow userId]
  --        , class_ \"logged-in-user-card\"
  --        ] do
  --       toHtml userName
  --
  -- demo :: HtmlT m ()
  -- demo = 'renderWidget' @B5 @LoggedInUser
  -- @
  --
  class RenderWidget t w m where
    renderWidget :: HtmlT m ()


-- vim:set ft=haskell sw=2 ts=2 et:
