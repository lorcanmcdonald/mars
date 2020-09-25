{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Cat (Cat (..)) where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as ByteString
import Data.String.Conv
import qualified Data.Text as Text
import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Query
import Mars.Types
import Test.QuickCheck

newtype Cat = Cat [Query]
  deriving (Generic, Show, Eq, Typeable)

instance Command Cat where
  evalCommand s (Cat []) =
    ( s,
      Output . toS
        . encodePretty
        . queryDoc (path s)
        . document
        $ s
    )
  evalCommand s (Cat l) =
    ( s,
      Output . toS . ByteString.intercalate "\n" $
        (=<<) formattedJSONText l
    )
    where
      formattedJSONText :: Query -> [ByteString.ByteString]
      formattedJSONText q =
        fmap encodePretty
          . queryDoc (path s <> q)
          . document
          $ s
  printCommand = error "printCommand"
  renderCommand (Cat l) = Text.intercalate " " $ "cat" : (renderQuery <$> l)

instance Arbitrary Cat where
  arbitrary = Cat <$> arbitrary
