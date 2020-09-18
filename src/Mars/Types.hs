{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mars.Types
  ( ANSIColour (..),
    Command (..),
    DirectoryEntry (..),
    GlobItem (..),
    ItemName (..),
    ItemType (..),
    MarsState (..),
    Query (..),
    QueryItem (..),
    UnnormalizedQueryItem (..),
  )
where

import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import GHC.Generics

-- | The datatype representing the queries possible for commands that select
--  - items
--  -
data Query
  = DefaultLocation
  | Query (NonEmpty QueryItem)
  deriving (Generic, Show, Eq)

-- | A data type representing the primitive commands available in the Mars
-- repl
data Command
  = Cat [Query]
  | Ls Query
  | Save Text.Text
  | Load Text.Text
  | Update Query Value
  | Cd Query
  | Pwd
  deriving (Generic, Show, Eq, Typeable)

instance Semigroup Query where
  DefaultLocation <> b = b
  Query _ <> DefaultLocation = DefaultLocation
  (Query a) <> (Query b) = Query (a <> b)

instance Monoid Query where
  mempty = DefaultLocation

data UnnormalizedQueryItem
  = GlobInput (NonEmpty GlobItem)
  | LevelAbove
  | CurrentLevel
  deriving (Generic, Show, Eq)

newtype QueryItem
  = Glob (NonEmpty GlobItem)
  deriving (Generic, Show, Eq)

data GlobItem
  = AnyChar
  | AnyCharMultiple
  | LiteralString Text.Text
  deriving (Generic, Show, Eq)

-- | The state of the replay program
data MarsState = MarsState
  { path :: Query,
    document :: Maybe Value
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
