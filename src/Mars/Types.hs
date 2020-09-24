{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mars.Types
  ( ANSIColour (..),
    DirectoryEntry (..),
    ItemName (..),
    ItemType (..),
    MarsState (..),
    Output (..),
  )
where

import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import Mars.Query

newtype Output = Output Text
  deriving (Show, Eq)

-- | The state of the replay program
data MarsState = MarsState
  { path :: Query,
    document :: Value
  }
  deriving (Generic)

data ANSIColour
  = Grey
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Show, Eq)

data DirectoryEntry = DirectoryEntry ItemName ItemType
  deriving (Show, Eq)

newtype ItemName = ItemName Text
  deriving (Show, Eq)

data ItemType
  = MarsObject
  | MarsList
  | MarsString
  | MarsNumber
  | MarsBool
  | MarsNull
  deriving (Show, Eq)
