{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Mars.Arbitraries where

#if __GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Aeson
import qualified Data.HashMap.Lazy as Map
import Data.String.Conv
import Data.Text (Text)
-- import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Mars.Parser
import Mars.Types
import Test.QuickCheck

-- instance Arbitrary Text.Text where
--   arbitrary = Text.pack <$> arbString

arbString :: Gen Text
arbString =
  toS
    <$> listOf
      (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)

arbDict :: Gen [(Text, Text)]
arbDict = listOf stupple

stupple :: Gen (Text, Text)
stupple = (,) <$> arbString <*> arbString

arbPort :: Gen (Maybe Integer)
arbPort = oneof [pure Nothing]

arbitraryCommand :: Gen Operation
arbitraryCommand =
  oneof
    [ OpCat <$> arbitrary,
      OpLs <$> arbitrary,
      OpSave <$> arbitrary,
      OpLoad <$> arbitrary,
      OpSet <$> arbitrary,
      OpCd <$> arbitrary,
      OpPwd <$> arbitrary
    ]

arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = arbitrary `suchThat` (> 0)

instance Arbitrary MarsState where
  arbitrary = MarsState <$> arbitrary <*> arbitrary

arbitraryArray :: Gen Array
arbitraryArray = Vector.fromList <$> arbitrary

arbitraryObject :: Gen Object
arbitraryObject = Map.fromList <$> listOf ((,) <$> arbString <*> arbitrary)

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
