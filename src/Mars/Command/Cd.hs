{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Cd (Cd (..), CdResult (..)) where

import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Query (Query)
import Mars.Renderable
import Mars.Types
import Test.QuickCheck
import Prelude hiding (putStrLn)

newtype Cd = Cd Query
  deriving (Generic, Show, Eq, Typeable)

newtype CdResult = ChangePath Query

instance Command Cd CdResult where
  evalCommand s (Cd query) = ChangePath newQuery
    where
      newQuery
        | itemExists = path s <> query
        | otherwise = path s
      itemExists = not . null . queryDoc (path s <> query) . document $ s

instance Action CdResult where
  execCommand state (ChangePath newQuery) =
    pure $ state {path = newQuery}

instance Renderable Cd where
  render (Cd a) = "cd " <> render a

instance Arbitrary Cd where
  arbitrary = Cd <$> arbitrary
