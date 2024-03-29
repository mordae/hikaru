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
  import Crude.Prelude

  import Data.ByteString.Parser.Char8
  import Data.List (filter, sortOn)


  -- |
  -- Single media element.
  --
  -- When used for media without a subtype (such as languages or encodings),
  -- the subType field is just @""@ while quality defaults to @1.0@.
  --
  data Media
    = Media
      { mainType       :: ByteString
      , subType        :: ByteString
      , params         :: [(ByteString, ByteString)]
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
  parseMedia :: ByteString -> Either String [Media]
  parseMedia = parseOnly (pMediaList <* endOfInput)


  -- |
  -- Parser for the media list coded mostly to the RFC 2045.
  --
  pMediaList :: Parser [Media]
  pMediaList = skipSpace *> (pMedia `sepBy` pSeparator)
    where
      pMedia :: Parser Media
      pMedia = do
        mainType <- pToken
        subType  <- (char '/' *> pToken) <|> pure ""

        (qs, params) <- partitionEithers <$> many (eitherP pQuality pParameter)

        let quality = fromMaybe 1.0 . listToMaybe $ qs

        return Media{..}

      pParameter :: Parser (ByteString, ByteString)
      pParameter = do
        _     <- char ';' <* skipSpace
        name  <- pToken
        _     <- char '=' <* skipSpace
        value <- pValue
        return (name, value)

      pQuality :: Parser Float
      pQuality = do
        _ <- char ';' <* skipSpace
        _ <- char 'q' <* skipSpace
        _ <- char '=' <* skipSpace
        fractional

      pToken :: Parser ByteString
      pToken = label "token" $ takeTill1 isSpecial <* skipSpace

      pSeparator :: Parser Char
      pSeparator = char ',' <* skipSpace

      pValue :: Parser ByteString
      pValue = branch [ (char '"', \_ -> pQuotedStr)
                         , (   pure ' ', \_ -> pToken)
                         ]

      pQuotedStr :: Parser ByteString
      pQuotedStr = pString <* char '"' <* skipSpace

      pString :: Parser ByteString
      pString = label "string" $ takeWhile isStrChar

      isStrChar :: (Char -> Bool)
      isStrChar c = c /= '\\' && c /= '"'

      isSpecial :: (Char -> Bool)
      isSpecial c = c <= ' '
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
  matchMedia l r = mainMatches && subMatches && r.quality > 0.0
    where
      mainMatches = l.mainType == r.mainType || r.mainType == "*"
      subMatches  = l.subType  == r.subType  || r.subType  == "*"


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
                        (l, r):_ -> Just $ l { quality = r.quality }
    where
      best = sortOn (negate . (.quality) . snd) good
      good = filter (uncurry matchMedia) prod
      prod = [(l, r) | l <- ls, r <- rs]


-- vim:set ft=haskell sw=2 ts=2 et:
