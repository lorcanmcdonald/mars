module Mars.Types where

import Data.Aeson.Types
import Network.URL
import qualified Data.Text as Text


-- |The datatype representing the queries possible for comamnds that select items
data Query = Query [QueryItem]
    deriving (Show, Eq)

data QueryItem = NamedItem (Text.Text)
               | IndexedItem (Int)
               | WildCardItem
               | LevelAbove
    deriving (Show, Eq)

data CollectionValue = O Object | A Array
    deriving (Show, Eq)

-- |A data type representing the primitive commands available in the Mars
-- repl
data Command  = Get (Maybe URL)
              | Cat [Query]
              | Ls (Maybe Query)
              | Save Text.Text
              | Load Text.Text
              | Update Query Value
              -- | Json -- Items return json types
              -- | Join -- Join two query results, where they refer to the same items
              | Login URL
              | Cd Query
              | Href
              | Pwd
              deriving (Show, Eq)

-- | The state of the replay program
data State = State   { url :: Maybe URL
                     , path :: Query
                     , document :: Maybe CollectionValue
                     }
                     deriving (Show, Eq)
