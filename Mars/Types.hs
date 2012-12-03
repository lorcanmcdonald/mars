{-#LANGUAGE DeriveDataTypeable #-}
module Mars.Types where

import Data.Typeable
import Data.Aeson.Types
import Network.URL
import Network.HTTP.Conduit
import qualified Data.Text as Text


-- |The datatype representing the queries possible for comamnds that select items
data Query = Query [QueryItem]
    deriving (Show, Eq, Typeable)

data QueryItem = NamedItem (Text.Text)
               | IndexedItem (Int)
               | WildCardItem
               | LevelAbove
    deriving (Show, Eq, Typeable)

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
              | Login URL [(String, String)]
              | Cd Query
              | Href
              | Pwd
              deriving (Show, Eq, Typeable)

-- | The state of the replay program
data State = State   { url :: Maybe URL
                     , path :: Query
                     , document :: Maybe Value
                     , cookies  :: CookieJar
                     }
