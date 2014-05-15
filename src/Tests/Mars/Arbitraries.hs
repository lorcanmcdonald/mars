{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Mars.Arbitraries where

import Data.Aeson
import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Number
import Mars.Types
import Test.QuickCheck
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbString

-- TODO we are explicitly not testing empty strings here, we really should
arbString :: Gen String
arbString = listOf ( elements (['A' .. 'Z'] <> ['a' .. 'z'] )) `suchThat` (not . null)

arbDict :: Gen [(String, String)]
arbDict = listOf stupple

stupple :: Gen (String, String)
stupple = (,) <$> arbString <*> arbString

instance Arbitrary Value where
    arbitrary = oneof [ Array <$> arbitrary
                      , String <$> arbitrary
                      -- , Number <$> arbitrary
                      , Bool <$> arbitrary
                      , pure Null
                      ]

-- Only creates list of length four to prevent runaway data structures
instance Arbitrary Array where
    arbitrary = Vector.fromListN 4 <$> listOf arbitrary

instance Arbitrary Number where
    arbitrary = oneof [ I <$> arbitrary
                      , D <$> arbitrary
                      ]

arbPort :: Gen (Maybe Integer)
arbPort = oneof [ pure Nothing ]

instance Arbitrary Command where
    arbitrary = oneof [ Cat <$> arbitrary
                      , Ls <$> arbitrary
                      , Save <$> arbitrary
                      , Load <$> arbitrary
                      , Update <$> arbitrary <*> arbitrary
                      , Cd <$> arbitrary
                      , pure Pwd
                      ]

-- TODO we are explicitly not testing empty strings here, we really should
instance Arbitrary Query where
    arbitrary = suchThat (Query <$> arbitrary) (\ (Query l) -> not $ null l)

instance Arbitrary QueryItem where
    arbitrary = oneof  [ NamedItem <$> arbitrary
                       , IndexedItem <$> arbitraryPositiveInt
                       , pure WildCardItem
                       , pure LevelAbove
                       ]

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = arbitrary `suchThat` (> 0)

instance Arbitrary MarsState where
    arbitrary = MarsState <$> arbitrary <*> arbitrary

arbitraryArray :: Gen Array
arbitraryArray = Vector.fromList <$> arbitrary

arbitraryObject :: Gen Object
arbitraryObject = Map.fromList <$> arbitrary
