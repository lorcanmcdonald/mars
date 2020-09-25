{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Pwd (Pwd (..)) where

import Data.Text.IO (putStrLn)
import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Types
import Test.QuickCheck
import Prelude hiding (putStrLn)

data Pwd = Pwd
  deriving (Generic, Show, Eq, Typeable)

instance Command Pwd where
  evalCommand s Pwd = (s, Output . renderQuery . path $ s)
  printCommand _ (state, Output o) = do
    putStrLn o
    return state
  renderCommand Pwd = "pwd"

instance Arbitrary Pwd where
  arbitrary = pure Pwd
