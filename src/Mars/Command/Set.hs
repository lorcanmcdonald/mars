{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Set (Set (..)) where

import Data.Aeson
import Data.String.Conv
import Data.Text (Text)
import Data.Typeable
import qualified Data.Vector as Vector
import GHC.Generics
import Mars.Command
import Mars.Query (Query)
import Mars.Types
import Test.QuickCheck

data Set = Set Query Value
  deriving (Generic, Show, Eq, Typeable)

instance Command Set where
  readCommand = error "readCommand"
  evalCommand s (Set query value) = (s {document = newDoc}, Output "")
    where
      newDoc :: Value
      newDoc = doUpdate . document $ s
      doUpdate :: Value -> Value
      doUpdate doc = modifyDoc doc query value
  printCommand = error "printCommand"
  renderCommand = error "renderCommand"

instance Arbitrary Set where
  arbitrary = Set <$> arbitrary <*> arbitrary

instance Arbitrary Value where
  arbitrary =
    oneof
      [ Array <$> arbitrary,
        String <$> arbString,
        -- , Number <$> arbitrary
        Bool <$> arbitrary,
        pure Null
      ]

-- Only creates list of length four to prevent runaway data structures
instance Arbitrary Array where
  arbitrary = Vector.fromListN 4 <$> listOf arbitrary

arbString :: Gen Text
arbString =
  toS
    <$> listOf
      (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)
