{-|
Module      :  Web.Hikaru.Media
Copyright   :  Jan Hamal Dvořák
License     :  AGPL-3

Maintainer  :  mordae@anilinux.org
Stability   :  unstable
Portability :  non-portable (ghc)

This module provides means for parsing HTTP media lists that can be found
in the headers such as @Accept@, @Accept-Charset@, @Accept-Encoding@ and
@Accept-Language@.
-}

module Web.Hikaru.Media
  ( Media(..)

    -- * Parsing
  , parseMedia

    -- * Matching
  , matchMedia
  , matchMediaList
  , selectMedia
  )
where
  import BasePrelude hiding (group, find)

  import Data.String.Conversions
  import Data.Text (Text)
  import Data.Text.ICU


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
  parseMedia = sortWith (negate . mediaQuality)
             . mapMaybe build
             . mapMaybe (group 0)
             . findAll reMedia
    where
      build :: Text -> Maybe Media
      build t = do
        match <- find reParts t
        main  <- group 1 match
        sub   <- group 2 match
        q     <- (readMaybe =<< cs <$> group 3 match) <|> Just 1.0

        return $ Media main sub q


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


  -- |
  -- Used to split text into individual media elements.
  --
  reMedia :: Regex
  reMedia = "(?:[^,]|\"(?:[^\"]|\\\")*\")+"


  -- |
  -- Used to split media element into its individual components.
  --
  reParts :: Regex
  reParts = "([[:alnum:]!#$%&'*+.^_`|~-]+)(?:\\s*/\\s*([[:alnum:]!#$%&'*+.^_`|~-]+))?(?:.*;q\\s*=\\s*([0-9.]+))?"


-- vim:set ft=haskell sw=2 ts=2 et:
