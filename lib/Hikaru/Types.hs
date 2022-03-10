-- |
-- Module      :  Hikaru.Types
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Hikaru.Types
  ( Param(..)
  , Severity(..)
  )
where
  import Praha

  import qualified Data.ByteString
  import qualified Data.ByteString.Lazy
  import qualified Data.Text.Encoding
  import qualified Data.Text.Encoding.Error
  import qualified Data.Text.Lazy

  -- |
  -- Types that can be parsed from a route segment or a query string
  -- parameter into some kind of value. One does not usually pass around
  -- more complex arguments than these, so forgive the limited menu.
  --
  class Param a where
    fromParam :: Text -> Maybe a
    toParam   :: a -> Text

  instance (Param a) => Param (Maybe a) where
    fromParam = Just . fromParam
    {-# INLINE fromParam #-}

    toParam = maybe "" toParam
    {-# INLINE toParam #-}

  instance Param Int where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Int8 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Int16 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Int32 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Int64 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Word where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Word8 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Word16 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Word32 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Word64 where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Integer where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Natural where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Float where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param Double where
    fromParam = readMaybe . cs
    {-# INLINE fromParam #-}

    toParam = cs . show
    {-# INLINE toParam #-}

  instance Param () where
    fromParam _ = Just ()
    {-# INLINE fromParam #-}

    toParam _ = ""
    {-# INLINE toParam #-}

  instance Param Bool where
    fromParam "true"  = Just True
    fromParam "True"  = Just True
    fromParam "yes"   = Just True
    fromParam "Yes"   = Just True
    fromParam "on"    = Just True
    fromParam "On"    = Just True
    fromParam "1"     = Just True
    fromParam "false" = Just False
    fromParam "False" = Just False
    fromParam "off"   = Just False
    fromParam "Off"   = Just False
    fromParam "no"    = Just False
    fromParam "No"    = Just False
    fromParam "0"     = Just False
    fromParam _else   = Nothing
    {-# INLINE fromParam #-}

    toParam True  = "true"
    toParam False = "false"
    {-# INLINE toParam #-}

  instance Param Char where
    fromParam inp = case (cs inp) of
                      [x]   -> Just x
                      _else -> Nothing
    {-# INLINE fromParam #-}

    toParam char = cs [char]
    {-# INLINE toParam #-}

  instance Param String where
    fromParam sp = Just (cs sp)
    {-# INLINE fromParam #-}

    toParam = cs
    {-# INLINE toParam #-}

  instance Param Text where
    fromParam sp = Just sp
    {-# INLINE fromParam #-}

    toParam = id
    {-# INLINE toParam #-}

  instance Param Data.Text.Lazy.Text where
    fromParam sp = Just $ Data.Text.Lazy.fromStrict sp
    {-# INLINE fromParam #-}

    toParam = Data.Text.Lazy.toStrict
    {-# INLINE toParam #-}

  instance Param Data.ByteString.ByteString where
    fromParam sp = Just $ Data.Text.Encoding.encodeUtf8 sp
    {-# INLINE fromParam #-}

    toParam = Data.Text.Encoding.decodeUtf8With
                Data.Text.Encoding.Error.lenientDecode
    {-# INLINE toParam #-}

  instance Param Data.ByteString.Lazy.ByteString where
    fromParam sp = Just $ Data.ByteString.Lazy.fromStrict
                        $ Data.Text.Encoding.encodeUtf8 sp
    {-# INLINE fromParam #-}

    toParam = Data.Text.Encoding.decodeUtf8With
                Data.Text.Encoding.Error.lenientDecode
            . Data.ByteString.Lazy.toStrict
    {-# INLINE toParam #-}


  -- |
  -- Information severity to be used for messages.
  --
  data Severity
    = Success
    | Warning
    | Danger
    deriving (Eq, Ord, Enum, Show, Generic)

  instance NFData Severity

  -- |
  -- Concatenation yields the higher severity.
  --
  instance Semigroup Severity where
    (<>) = max

  -- |
  -- 'Success' is the neutral element.
  --
  instance Monoid Severity where
    mempty = Success


-- vim:set ft=haskell sw=2 ts=2 et:
