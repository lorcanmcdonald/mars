{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Mars.Arbitraries where

import Control.Monad
import Data.Aeson
import Data.Attoparsec.Number
import Network.URL
import Mars.Types
import Test.QuickCheck
import Network.HTTP.Conduit
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

instance Arbitrary Text.Text where
    arbitrary = liftM Text.pack arbString

arbString :: Gen String
arbString = listOf ( elements (['A'..'Z'] ++ ['a' .. 'z'])) `suchThat` null a

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
    arbitrary = oneof [ liftM Absolute arbitrary
                      , return HostRelative
                      , return PathRelative
                      ]

instance Arbitrary Host where
    arbitrary = liftM3 Host arbitrary arbString arbPort

instance Arbitrary Protocol where
    arbitrary = oneof [ liftM HTTP arbitrary
                      , liftM FTP arbitrary
                      ]

instance Arbitrary Value where
    arbitrary = oneof [ liftM Array arbitrary
                      , liftM String arbitrary
                      , liftM Number arbitrary
                      , liftM Bool arbitrary
                      , return Null
                      ]

-- Only creates list of length four to prevent runaway data structures
instance Arbitrary Array where
    arbitrary = liftM (Vector.fromListN 4) $ listOf arbitrary

instance Arbitrary Number where
    arbitrary = oneof [ liftM I arbitrary
                      , liftM D arbitrary
                      ]

arbPort :: Gen (Maybe Integer)
arbPort = oneof [ return Nothing
                 ]

instance Arbitrary Command where
    arbitrary = oneof [ liftM Get arbitrary
                      , liftM Cat arbitrary
                      , liftM Ls arbitrary
                      , liftM Save arbitrary
                      , liftM Load arbitrary
                      , liftM2 Update arbitrary arbitrary
                      , liftM Cd arbitrary
                      , return Href
                      , return Pwd
                      ]

instance Arbitrary Query where
    arbitrary = suchThat (liftM Query arbitrary) (\ (Query l) -> null l)

instance Arbitrary QueryItem where
    arbitrary = oneof  [ liftM NamedItem arbitrary
                       , liftM IndexedItem arbitraryPositiveInt
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
arbitraryArray = liftM Vector.fromList arbitrary

arbitraryObject :: Gen Object
arbitraryObject = liftM Map.fromList arbitrary
