-- |
-- Module      :  Main
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Main
  ( main
  )
where
  import Crude.Prelude

  import Lucid qualified

  import Data.ByteString.Builder qualified as BSB
  import Data.ByteString.Lazy qualified as LBS

  import Hikaru qualified
  import Hikaru.HTML qualified as Html

  import Network.HTTP.Media qualified as NHMedia

  import Criterion.Main


  main :: IO ()
  main = defaultMain
    [ bgroup "text/plain, text/html;q=0.8"
      [ bench "Hikaru" $ nf (show . benchHikaruMedia) "text/plain, text/html;q=0.8"
      , bench "N.H.Media" $ nf (show . benchNHMedia) "text/plain, text/html;q=0.8"
      ]

    , bgroup "text/plain, text/html"
      [ bench "Hikaru" $ nf (show . benchHikaruMedia) "text/plain, text/html"
      , bench "N.H.Media" $ nf (show . benchNHMedia) "text/plain, text/html"
      ]

    , bgroup "text/html"
      [ bench "Hikaru" $ nf (show . benchHikaruMedia) "text/html"
      , bench "N.H.Media" $ nf (show . benchNHMedia) "text/html"
      ]
    , bgroup "templates"
      [ bench "Lucid" $ nfIO (benchLucid "Hello, World!")
      , bench "Hikaru.HTML" $ nfIO (benchHtml "Hello, World!")
      ]
    ]


  benchHikaruMedia :: Text -> Either String [Hikaru.Media]
  benchHikaruMedia media = Hikaru.parseMedia media


  benchNHMedia :: ByteString -> Maybe NHMedia.MediaType
  benchNHMedia media = NHMedia.parseAccept media


  benchLucid :: Text -> IO LBS.ByteString
  benchLucid greeting = do
    builder <- Lucid.execHtmlT do
      Lucid.doctypehtml_ do
        Lucid.head_ do
          Lucid.title_ $ Lucid.toHtml greeting

        Lucid.body_ do
          Lucid.h1_ $ Lucid.toHtml greeting

    return $ BSB.toLazyByteString builder


  benchHtml :: Text -> IO LBS.ByteString
  benchHtml greeting = do
    builder <- Html.fromHtmlT do
      Html.doctype
      Html.tag "html" "" do
        Html.tag "head" "" do
          Html.tag "title" "" do
            Html.text greeting

        Html.tag "body" "" do
          Html.tag "h1" "" do
            Html.text greeting

    return $ BSB.toLazyByteString builder


-- vim:set ft=haskell sw=2 ts=2 et:
