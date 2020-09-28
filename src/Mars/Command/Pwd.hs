{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Pwd (Pwd (..)) where

import Data.Text (Text)
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

newtype PwdResult = Print Text

instance Command Pwd PwdResult where
  evalCommand s Pwd = Print . render . path $ s

instance Action PwdResult where
  execCommand state (Print o) = do
    putStrLn o
    return state

instance Renderable Pwd where
  render Pwd = "pwd"

instance Arbitrary Pwd where
  arbitrary = pure Pwd
