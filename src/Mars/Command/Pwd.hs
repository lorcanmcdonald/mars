{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Pwd (Pwd (..)) where

import Data.Typeable
import GHC.Generics
import Mars.Command
import Test.QuickCheck

data Pwd = Pwd
  deriving (Generic, Show, Eq, Typeable)

instance Command Pwd where
  evalCommand = error "evalCommand"
  printCommand = error "printCommand"
  renderCommand Pwd = "pwd"

instance Arbitrary Pwd where
  arbitrary = pure Pwd
