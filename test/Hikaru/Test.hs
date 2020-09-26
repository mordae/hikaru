{-|
Module      :  Hikaru.Test
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

Common testing definitions.
-}

module Hikaru.Test
  ( get
  , post

  , module Network.HTTP.Types
  , module Network.Wai
  , module Network.Wai.Internal
  , module Network.Wai.Test
  , module Test.Hspec
  )
where
  import Relude hiding (get)

  import qualified Data.ByteString.Lazy as LBS

  import Network.HTTP.Types
  import Network.Wai
  import Network.Wai.Internal
  import Network.Wai.Test
  import Test.Hspec


  get :: ByteString -> RequestHeaders -> Session SResponse
  get path headers =
    let req = setPath defaultRequest path
     in request $ req { requestMethod = methodGet
                      , requestHeaders = headers
                      }


  post :: ByteString -> RequestHeaders -> ByteString -> Session SResponse
  post path headers body = srequest sreq
    where
      req = setPath defaultRequest path
      sreq = SRequest { simpleRequestBody = LBS.fromStrict body
                      , simpleRequest     = req { requestMethod  = methodPost
                                                , requestHeaders = headers
                                                }
                      }


-- vim:set ft=haskell sw=2 ts=2 et:
