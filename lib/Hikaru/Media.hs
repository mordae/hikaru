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
  import Praha

  import Data.List (filter, lookup, sortOn)

  import Data.Char
  import Data.Attoparsec.Text


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
                       Left reason -> error reason
                       Right []    -> error "no media given"
                       Right (m:_) -> m


  -- |
  -- Try to parse a comma-separated media type list.
  --
  -- Example:
  --
  -- >>> parseMedia "text/html, text/plain;q=0.7"
  -- [ Media { mainType = "text", subType = "html", quality = 1.0, params = [] }
  -- , Media { mainType = "text", subType = "plain", quality = 0.7, params = [] }
  -- ]
  --
  parseMedia :: Text -> Either String [Media]
  parseMedia = parseOnly (pMediaList <* endOfInput)


  -- |
  -- Parser for the media list coded mostly to the RFC 2045.
  --
  pMediaList :: Parser [Media]
  pMediaList = pMedia `sepBy` pSeparator
    where
      pMedia :: Parser Media
      pMedia = do
        mainType <- pToken
        subType  <- (char '/' *> pToken) <|> string ""
        params   <- many' pParameter

        let quality = fromMaybe 1.0 do
                        q <- lookup "q" params
                        readMaybe (cs q)

        return Media{..}

      pParameter :: Parser (Text, Text)
      pParameter = do
        _     <- pSpaced $ char ';'
        name  <- pToken
        _     <- pSpaced $ char '='
        value <- pValue
        return (name, value)

      pToken :: Parser Text
      pToken = do
        pSpaced $ takeTill isSpecial

      pSeparator :: Parser Char
      pSeparator = pSpaced $ char ','

      pValue :: Parser Text
      pValue = pToken <|> pQuotedStr

      pQuotedStr :: Parser Text
      pQuotedStr = pSpaced $ pQuoted $ takeTill (== '\\')

      pSpaced :: Parser a -> Parser a
      pSpaced p = skipSpace *> p <* skipSpace

      pQuoted :: Parser a -> Parser a
      pQuoted p = char '"' *> p <* char '"'

      isSpecial :: (Char -> Bool)
      isSpecial c = isControl c || isSpace c
                    || c == '(' || c == ')'
                    || c == '<' || c == '>'
                    || c == '@' || c == ',' || c == ';'
                    || c == ':' || c == '\\'
                    || c == '"' || c == '/'
                    || c == '[' || c == ']'
                    || c == '?' || c == '='


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
