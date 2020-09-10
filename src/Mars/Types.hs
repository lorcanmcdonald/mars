{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Mars.Types where

import Data.Aeson.Types
import Data.List.NonEmpty
import qualified Data.Text as Text
import Data.Typeable
import GHC.Generics

-- | The datatype representing the queries possible for commands that select
--  - items
--  -
data Query = Query [QueryItem]
  deriving (Generic, Show, Eq)

instance Monoid Query where
  mempty = Query []

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
  (Query a) <> (Query b) = Query (a <> b)

data QueryItem
  = Glob (NonEmpty GlobItem)
  | LevelAbove
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
