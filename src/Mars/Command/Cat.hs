{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Cat (Cat (..)) where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as ByteString
import Data.String.Conv
import qualified Data.Text as Text
import Data.Text.IO (putStrLn)
import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Query
import Mars.Renderable
import Mars.Types
import Test.QuickCheck
import Prelude hiding (putStrLn)

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
  printCommand _ (state, Output o) = do
    putStrLn o
    return state

instance Renderable Cat where
  render (Cat l) = Text.intercalate " " $ "cat" : (render <$> l)

instance Arbitrary Cat where
  arbitrary = Cat <$> arbitrary
