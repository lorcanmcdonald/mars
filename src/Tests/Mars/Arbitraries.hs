{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Mars.Arbitraries where

#if __GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Aeson
import qualified Data.HashMap.Lazy as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Mars.Types
import Test.QuickCheck
import qualified Test.QuickCheck.Modifiers as Modifiers

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbString

-- TODO we are explicitly not testing empty strings here, we really should
arbString :: Gen String
arbString =
  listOf
    (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)

arbDict :: Gen [(String, String)]
arbDict = listOf stupple

stupple :: Gen (String, String)
stupple = (,) <$> arbString <*> arbString

instance Arbitrary Value where
  arbitrary =
    oneof
      [ Array <$> arbitrary,
        String <$> arbitrary,
        -- , Number <$> arbitrary
        Bool <$> arbitrary,
        pure Null
      ]

-- Only creates list of length four to prevent runaway data structures
instance Arbitrary Array where
  arbitrary = Vector.fromListN 4 <$> listOf arbitrary

arbPort :: Gen (Maybe Integer)
arbPort = oneof [pure Nothing]

instance Arbitrary Command where
  arbitrary =
    oneof
      [ Cat <$> arbitrary,
        Ls <$> arbitrary,
        Save <$> arbitrary,
        Load <$> arbitrary,
        Update <$> arbitrary <*> arbitrary,
        Cd <$> arbitrary,
        pure Pwd
      ]

instance Arbitrary Query where
  arbitrary = Query . NonEmpty.fromList . Modifiers.getNonEmpty <$> arbitrary

genGlob :: Gen (NonEmpty GlobItem)
genGlob = do
  startGlob <- Modifiers.getNonEmpty <$> arbitrary
  specialGlob <- oneof [pure AnyChar, pure AnyCharMultiple]
  endGlob <- Modifiers.getNonEmpty <$> arbitrary
  return . NonEmpty.fromList $ (startGlob <> [specialGlob] <> endGlob)

instance Arbitrary QueryItem where
  arbitrary =
    oneof
      [ Glob <$> genGlob
      ]

instance Arbitrary GlobItem where
  arbitrary =
    oneof [pure AnyChar, pure AnyCharMultiple]

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = arbitrary `suchThat` (> 0)

instance Arbitrary MarsState where
  arbitrary = MarsState <$> arbitrary <*> arbitrary

arbitraryArray :: Gen Array
arbitraryArray = Vector.fromList <$> arbitrary

arbitraryObject :: Gen Object
arbitraryObject = Map.fromList <$> arbitrary

instance Arbitrary ANSIColour where
  arbitrary =
    oneof
      [ pure Grey,
        pure Red,
        pure Green,
        pure Yellow,
        pure Blue,
        pure Magenta,
        pure Cyan,
        pure White
      ]
