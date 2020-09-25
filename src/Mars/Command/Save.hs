{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Save (Save (..)) where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.String.Conv
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Renderable
import Mars.Types
import Test.QuickCheck

newtype Save = Save Text
  deriving (Generic, Show, Eq, Typeable)

instance Command Save where
  evalCommand s (Save filename) = (s, Output filename)
  printCommand _ (s, Output filename) = s <$ writeFile (toS filename) jsonString
    where
      jsonString = toS . encodePretty $ toJSON s

instance Renderable Save where
  render (Save f) = "save " <> f

instance Arbitrary Save where
  arbitrary = Save <$> arbString

arbString :: Gen Text
arbString =
  toS
    <$> listOf
      (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)
