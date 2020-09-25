{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Pwd (Pwd (..)) where

import Data.Text.IO (putStrLn)
import Data.Typeable
import GHC.Generics
import Mars.Command
import Mars.Renderable
import Mars.Types
import Test.QuickCheck
import Prelude hiding (putStrLn)

data Pwd = Pwd
  deriving (Generic, Show, Eq, Typeable)

instance Command Pwd where
  evalCommand s Pwd = (s, Output . render . path $ s)
  printCommand _ (state, Output o) = do
    putStrLn o
    return state

instance Renderable Pwd where
  render Pwd = "pwd"

instance Arbitrary Pwd where
  arbitrary = pure Pwd
