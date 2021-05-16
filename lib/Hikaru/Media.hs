{-|
Module      :  Hikaru.Media
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides means for parsing HTTP media lists that can be found
in the headers such as @Accept@, @Accept-Charset@, @Accept-Encoding@ and
@Accept-Language@.
-}

module Hikaru.Media
  ( Media(..)

    -- * Parsing
  , parseMedia
  , pMediaList

    -- * Matching
  , matchMedia
  , matchMediaList
  , selectMedia
  )
where
  import Relude hiding (head, get, many)
  import Relude.Unsafe (head)
  import Text.ParserCombinators.ReadP
  import Data.String.Conversions
  import Data.Text (toLower)
  import Data.List (lookup)
  import Data.Char (isControl, isSpace)


  -- |
  -- Single media element.
  --
  -- When used for media without a subtype (such as languages or encodings),
  -- the respective field is just @""@ while quality defaults to @1.0@.
  --
  data Media
    = Media
      { mediaMainType  :: Text
      , mediaSubType   :: Text
      , mediaParams    :: [(Text, Text)]
      , mediaQuality   :: Float
      }
    deriving (Show, Eq, Ord)

  -- |
  -- You can construct media elements using @OverloadedStrings@.
  -- Just be careful - invalid syntax means a runtime error.
  --
  instance IsString Media where
    fromString = head . parseMedia . cs


  -- |
  -- Try to parse a comma-separated media type list.
  -- Media that fail to parse are simply omitted.
  --
  -- Example:
  --
  -- >>> parseMedia "text/html, text/plain;q=0.7"
  -- [ Media { mediaMainType = "text", mediaSubType = "html",  mediaQuality = 1.0 }
  -- , Media { mediaMainType = "text", mediaSubType = "plain", mediaQuality = 0.7 }
  -- ]
  --
  parseMedia :: Text -> [Media]
  parseMedia text = case readP_to_S pMediaList (cs (toLower text)) of
                      (m, ""):_ -> sortWith (negate . mediaQuality) m
                      _else     -> []


  -- |
  -- Parser for the media list coded mostly to the RFC 2045.
  -- Input is always lowercased and unicode is accepted.
  --
  pMediaList :: ReadP [Media]
  pMediaList = sepBy pMedia pSeparator <* eof
    where
      pMedia = do
        mediaMainType <- cs <$> pToken
        _             <- char '/'
        mediaSubType  <- cs <$> pToken
        mediaParams   <- many pParameter

        let mediaQuality = fromMaybe 1.0 do
                             q <- lookup "q" mediaParams
                             readMaybe (cs q)

        return Media{..}

      pParameter = do
        _     <- pSpaced $ char ';'
        name  <- cs <$> pToken
        _     <- pSpaced $ char '='
        value <- cs <$> pValue
        return (name, value)

      pSeparator = pSpaced $ char ','
      pToken     = pSpaced $ many1 (satisfy (not . quote))
      pValue     = pToken <++ pQuotedStr
      pQuotedStr = pSpaced $ pQuoted $ many (pExcept '\\')

      pExcept c  = satisfy (c /=)
      pSpaced p  = skipSpaces *> p <* skipSpaces
      pQuoted p  = char '"' *> p <* char '"'

      quote c    = isControl c || isSpace c || c `elem` specials
      specials   = "()<>@,;:\\\"/[]?=" :: [Char]


  -- |
  -- Checks whether the provided media (on the left side)
  -- matches the requested media (on the right side).
  --
  -- Media match when the main and sub parts are equal and quality on
  -- the right side is greated than zero. Additionaly, both main and
  -- sub @*@ tokens on the right side match anything on the left side.
  --
  -- Example:
  --
  -- >>> matchMedia "text/html" "text/*"
  -- True
  -- >>> matchMedia "text/html" "image/*"
  -- False
  --
  matchMedia :: Media -> Media -> Bool
  matchMedia l r = mainMatches && subMatches && mediaQuality r > 0.0
    where
      mainMatches = mediaMainType l == mediaMainType r
                    || mediaMainType r == "*"

      subMatches  = mediaSubType l == mediaSubType r
                    || mediaSubType r == "*"


  -- |
  -- Checks whether the provided media (on the left side)
  -- matches any of the requested media (on the right side).
  --
  -- Example:
  --
  -- >>> matchMediaList "text/plain" ["text/html", "*/*;q=0.01"]
  -- True
  -- >>> matchMediaList "text/plain" ["text/html", "*/*;q=0"]
  -- False
  --
  matchMediaList :: Media -> [Media] -> Bool
  matchMediaList l rs = filter (matchMedia l) rs /= []


  -- |
  -- Given a provided media list (on the left side) and a requested media
  -- list (on the right side), produce the provided media that matches
  -- the requested media the best and update the quality to reflect this.
  --
  selectMedia :: [Media] -> [Media] -> Maybe Media
  selectMedia ls rs = case best of
                        [ ]      -> Nothing
                        (l, r):_ -> Just $ l { mediaQuality = mediaQuality r }
    where
      best = sortWith (negate . mediaQuality . snd) good
      good = filter (uncurry matchMedia) prod
      prod = [(l, r) | l <- ls, r <- rs]


-- vim:set ft=haskell sw=2 ts=2 et:
