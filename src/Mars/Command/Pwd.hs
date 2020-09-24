{-# LANGUAGE DeriveGeneric #-}

module Mars.Command.Pwd (Pwd (..)) where

import Data.Typeable
import GHC.Generics
import Mars.Command
import Test.QuickCheck
import Text.ParserCombinators.Parsec hiding ((<|>))

data Pwd = Pwd
  deriving (Generic, Show, Eq, Typeable)

instance Command Pwd where
  readCommand = Pwd <$ string "pwd"
  evalCommand = error "evalCommand"
  printCommand = error "printCommand"
  renderCommand = error "renderCommand"

instance Arbitrary Pwd where
  arbitrary = pure Pwd
