{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Mars.Arbitraries where

import Control.Monad
import Data.Aeson
import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Number
import Network.URL
import Mars.Types
import Test.QuickCheck
import Network.HTTP.Conduit
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

instance Arbitrary Text.Text where
    arbitrary = Text.pack <$> arbString

arbString :: Gen String
arbString = listOf ( elements (['A'..'Z'] `mappend` ['a' .. 'z'])) `suchThat` null

instance Arbitrary URL where
    arbitrary = liftM3 URL arbitrary arbString arbDict

arbDict :: Gen [(String, String)]
arbDict = listOf stupple

stupple :: Gen (String, String)
stupple = do
            x <- arbString
            y <- arbString
            return (x, y)

instance Arbitrary URLType where
    arbitrary = oneof [ Absolute <$> arbitrary
                      , return HostRelative
                      , return PathRelative
                      ]

instance Arbitrary Host where
    arbitrary = liftM3 Host arbitrary arbString arbPort

instance Arbitrary Protocol where
    arbitrary = oneof [ HTTP <$> arbitrary
                      , FTP <$> arbitrary
                      ]

instance Arbitrary Value where
    arbitrary = oneof [ Array <$> arbitrary
                      , String <$> arbitrary
                      , Number <$> arbitrary
                      , Bool <$> arbitrary
                      , return Null
                      ]

-- Only creates list of length four to prevent runaway data structures
instance Arbitrary Array where
    arbitrary = (Vector.fromListN 4) <$> listOf arbitrary

instance Arbitrary Number where
    arbitrary = oneof [ I <$> arbitrary
                      , D <$> arbitrary
                      ]

arbPort :: Gen (Maybe Integer)
arbPort = oneof [ return Nothing
                 ]

instance Arbitrary Command where
    arbitrary = oneof [ Get <$> arbitrary
                      , Cat <$> arbitrary
                      , Ls <$> arbitrary
                      , Save <$> arbitrary
                      , Load <$> arbitrary
                      , liftM2 Update arbitrary arbitrary
                      , Cd <$> arbitrary
                      , return Href
                      , return Pwd
                      ]

instance Arbitrary Query where
    arbitrary = suchThat (Query <$> arbitrary) (\ (Query l) -> null l)

instance Arbitrary QueryItem where
    arbitrary = oneof  [ NamedItem <$> arbitrary
                       , IndexedItem <$> arbitraryPositiveInt
                       , return WildCardItem
                       , return LevelAbove
                       ]

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = arbitrary `suchThat` (> 0)

instance Arbitrary State where
    arbitrary = liftM4 State arbitrary arbitrary arbitrary arbitrary

instance Arbitrary CookieJar where
    arbitrary = oneof []

arbitraryArray :: Gen Array
arbitraryArray = Vector.fromList <$> arbitrary

arbitraryObject :: Gen Object
arbitraryObject = Map.fromList <$> arbitrary
