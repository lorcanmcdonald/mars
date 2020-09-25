{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Cd (Cd (..)) where

import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Query (Query)
import Mars.Types
import Test.QuickCheck

-- import Text.ParserCombinators.Parsec hiding ((<|>))

newtype Cd = Cd Query
  deriving (Generic, Show, Eq, Typeable)

instance Command Cd where
  evalCommand s (Cd query) =
    let newState = s {path = newQuery}
     in (newState, Output "")
    where
      newQuery
        | itemExists = path s
        | otherwise = path s <> query
      itemExists =
        null . queryDoc (path s <> query)
          . document
          $ s
  printCommand = error "printCommand"
  renderCommand (Cd a) = "cd " <> renderQuery a

instance Arbitrary Cd where
  arbitrary = Cd <$> arbitrary
