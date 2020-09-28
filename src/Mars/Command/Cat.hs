{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Cat (Cat (..)) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.String.Conv
import qualified Data.Text as Text
import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Query
import Mars.Renderable
import Mars.Types
import Test.QuickCheck

newtype Cat = Cat [Query]
  deriving (Generic, Show, Eq, Typeable)

newtype CatResult = PrintValue [Value]

instance Command Cat CatResult where
  evalCommand s (Cat []) =
    PrintValue $ queryDoc (path s) . document $ s
  evalCommand s (Cat l) =
    PrintValue $ concatMap (\q -> queryDoc (path s <> q) . document $ s) l

instance Action CatResult where
  execCommand state (PrintValue v) = do
    putStrLn . unlines $ toS . encodePretty <$> v
    return state

instance Renderable Cat where
  render (Cat l) = Text.intercalate " " $ "cat" : (render <$> l)

instance Arbitrary Cat where
  arbitrary = Cat <$> arbitrary
