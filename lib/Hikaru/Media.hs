-- |
-- Module      :  Hikaru.Media
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides means for parsing HTTP media lists that can be found
-- in headers such as @Accept@, @Accept-Charset@, @Accept-Encoding@ and
-- @Accept-Language@.
--

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
  import Praha hiding (many)

  import Data.Text (toLower)

  import Data.List (filter, lookup, sortOn)
  import Text.ParserCombinators.ReadP
  import Data.Char (isControl, isSpace)


  -- |
  -- Single media element.
  --
  -- When used for media without a subtype (such as languages or encodings),
  -- the subType field is just @""@ while quality defaults to @1.0@.
  --
  data Media
    = Media
      { mainType       :: Text
      , subType        :: Text
      , params         :: [(Text, Text)]
      , quality        :: Float
      }
    deriving (Show, Eq, Ord, Generic)

  instance NFData Media

  -- |
  -- You can construct media elements using @OverloadedStrings@.
  -- Just be careful - invalid syntax means a runtime error.
  --
  instance IsString Media where
    fromString str = case parseMedia (cs str) of
                       m:_   -> m
                       _else -> error $ "Failed to parse media " <> show str


  -- |
  -- Try to parse a comma-separated media type list.
  -- Media that fail to parse are simply omitted.
  --
  -- Example:
  --
  -- >>> parseMedia "text/html, text/plain;q=0.7"
  -- [ Media { mainType = "text", subType = "html", quality = 1.0, params = [] }
  -- , Media { mainType = "text", subType = "plain", quality = 0.7, params = [] }
  -- ]
  --
  parseMedia :: Text -> [Media]
  parseMedia text = case readP_to_S pMediaList (cs (toLower text)) of
                      (m, ""):_ -> sortOn (negate . quality) m
                      _else     -> []


  -- |
  -- Parser for the media list coded mostly to the RFC 2045.
  -- Input is always lowercased and unicode is accepted.
  --
  pMediaList :: ReadP [Media]
  pMediaList = sepBy pMedia pSeparator <* eof
    where
      pMedia = do
        mainType <- cs <$> pToken
        _        <- char '/'
        subType  <- cs <$> pToken
        params   <- many pParameter

        let quality = fromMaybe 1.0 do
                        q <- lookup "q" params
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
  matchMedia l r = mainMatches && subMatches && quality r > 0.0
    where
      mainMatches = mainType l == mainType r || mainType r == "*"
      subMatches  =  subType l == subType r  ||  subType r == "*"


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
                        (l, r):_ -> Just $ l { quality = quality r }
    where
      best = sortOn (negate . quality . snd) good
      good = filter (uncurry matchMedia) prod
      prod = [(l, r) | l <- ls, r <- rs]


-- vim:set ft=haskell sw=2 ts=2 et:
