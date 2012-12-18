{-#LANGUAGE DeriveDataTypeable #-}
module Mars.Types where

import Data.Monoid
import Data.Typeable
import Data.Aeson.Types
import Network.URL
import Network.HTTP.Conduit
import qualified Data.Text as Text


-- |The datatype representing the queries possible for commands that select items
data Query = Query [ QueryItem ]
    deriving (Show, Eq)

instance Monoid Query where
    mempty = Query []
    mappend (Query a) (Query b) = Query (a <> b)

data QueryItem = NamedItem (Text.Text)
               | IndexedItem (Int)
               | WildCardItem
               | LevelAbove
    deriving (Show, Eq)

-- |A data type representing the primitive commands available in the Mars
-- repl
data Command  = Get (Maybe URL)
              | Cat [Query]
              | Ls Query
              | Save Text.Text
              | Load Text.Text
              | Update Query Value
              | Login URL [(String, String)]
              | Cd Query
              | Href
              | Pwd
              deriving (Show, Eq, Typeable)

-- | The state of the replay program
data MarsState = MarsState   { url      :: Maybe URL
                     , path     :: Query
                     , document :: Maybe Value
                     , cookies  :: CookieJar
                     }
