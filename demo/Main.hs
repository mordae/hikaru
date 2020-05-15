{-|
Module      :  Main
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)
-}

module Main ( main
            )
where
  import BasePrelude

  import Hikaru.Demo (makeDemo)


  data Options
    = Options
      { optMainFunc    :: Options -> IO ()
      , optConf        :: String
      }


  mainFunc :: Parser (Options -> IO ())
  mainFunc = flag' mainVersion ( long "version"
                                 <> short 'V'
                                 <> help "Show version information"
                                 <> hidden
                               )
             <|> pure mainWebsite


  options :: Parser Options
  options = Options <$> mainFunc
                    <*> strOption ( long "config"
                                    <> short 'C'
                                    <> help "Configuration file to read"
                                    <> value "byrocraft.cfg"
                                  )


  main :: IO ()
  main = do
    opts <- execParser $
      info (helper *> options)
           ( fullDesc
             <> progDesc "Collaborative Administration Tool"
             <> footer "Report bugs at <http://github.com/mordae/byrocraft/issues>."
           )

    optMainFunc opts opts


  mainVersion :: Options -> IO ()
  mainVersion _opts = do
    prog <- getProgName
    putStrLn $ prog <> " " <> packageVersion


  mainWebsite :: Options -> IO ()
  mainWebsite Options{optConf} = serve =<< load [ Required optConf ]


-- vim:set ft=haskell sw=2 ts=2 et:
