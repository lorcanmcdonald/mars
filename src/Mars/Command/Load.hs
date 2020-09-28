{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Load (Load (..)) where

import Data.Aeson as Aeson
import Data.String.Conv
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Mars.Command
import System.IO (hPutStrLn, stderr)
import Test.QuickCheck
import Mars.Renderable

newtype Load = Load Text
  deriving (Generic, Show, Eq, Typeable)

newtype LoadResult = LoadFile Text

instance Command Load LoadResult where
  evalCommand _ (Load filename) = LoadFile filename

instance Action LoadResult where
  execCommand s (LoadFile filename) = do
    c <- readFile . toS $ filename
    (loadResult . decode . toS) c
    where
      loadResult Nothing = printErr "Could not parse"
      loadResult (Just j) = reportResult . fromJSON $ j
      reportResult (Aeson.Error err) = printErr err
      reportResult (Aeson.Success state) = pure state
      printErr err = s <$ hPutStrLn stderr ("Invalid saved state: " <> err)

instance Renderable Load where
  render (Load f) = "load \"" <> f <> "\""

instance Arbitrary Load where
  arbitrary = Load <$> arbString

arbString :: Gen Text
arbString =
  toS
    <$> listOf
      (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)
