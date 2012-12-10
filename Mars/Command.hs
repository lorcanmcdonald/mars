-- |Types representing items entered at the Mars command line
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Mars.Command

where

import Prelude hiding (id, (.))
import Control.Category

import Control.Applicative
import Data.Lens.Common
import Data.Aeson
import Data.Maybe
import Data.Monoid
-- import Data.Aeson.Lens
import qualified Data.Vector as Vector
import Network.URL
import Mars.Types
import qualified Network.HTTP.Conduit as HTTP
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text

prependToQuery :: Query -> Query -> Query
prependToQuery (Query a) (Query b) = Query (a `mappend` b)

-- | The initial state
initialState :: State
initialState = State { url         = Nothing
                     , path        = Query []
                     , document    = Nothing
                     , cookies     = HTTP.def
                     }

-- |Output a command in a format similar to how it would have be entered by the user
renderCommand :: Command -> Text.Text
renderCommand (Get Nothing)  = "get"
renderCommand (Get (Just u)) = "get " `mappend` Text.pack ( exportURL u)
renderCommand (Cat [])       = "cat"
renderCommand (Cat l)        = "cat " `mappend` Text.intercalate " " (renderQuery <$> l)
renderCommand (Ls a)         = "ls " `mappend` renderQuery a
renderCommand (Save f)       = "save " `mappend` f
renderCommand (Load f)       = "load \"" `mappend` f `mappend` "\""
renderCommand (Update q val) = "update " |++| renderQuery q |++| " " |++| Text.pack ( ByteString.unpack $ encode val)
renderCommand Href           = "href"
renderCommand Pwd            = "pwd"
renderCommand (Cd a)         = "cd " `mappend` renderQuery a

(|++|) :: Text.Text -> Text.Text -> Text.Text
(|++|) = Text.append

-- |Output a query in a format that would have been entered in the interpreter
renderQuery :: Query -> Text.Text
renderQuery (Query l) = Text.intercalate "/" (renderQueryItem <$> l)

-- |A text version of a QueryItem
renderQueryItem :: QueryItem -> Text.Text
renderQueryItem (NamedItem n)   = n
renderQueryItem (IndexedItem i) = Text.pack (show i)
renderQueryItem WildCardItem    = Text.pack "*"
renderQueryItem LevelAbove      = Text.pack ".."

emptyObjectCollection :: Value
emptyObjectCollection = object []

getC :: QueryItem -> Value -> Value
getC (IndexedItem i) (Array a) = fromMaybe (object []) $ (Vector.!?) a i
getC (NamedItem n) (Object o)  = fromMaybe (object []) $ Map.lookup n o
getC _ _                       = object []

setC :: QueryItem -> Value -> Value -> Value
setC (IndexedItem i) (v) (Array a) = toJSON . Vector.update a . Vector.fromList $ [(i, v)]
setC (NamedItem n) (v) (Object o)  = toJSON $ Map.insert n v o
setC _ _ c                   = c

moveUp :: Query -> Query
moveUp (Query q) =  Query . reverse . drop 1 $ reverse q

simplifyQuery :: Query -> Query
simplifyQuery (Query l) = Query . reverse . foldr simplify [] $ reverse l
    where
        simplify :: QueryItem -> [QueryItem] -> [QueryItem]
        simplify (LevelAbove) processed    = drop 1 processed
        simplify item processed   = item:processed

modifyDoc :: Value -> Query -> Value -> Value
modifyDoc cv q = modifyFunc q cv

queryDoc :: Value -> Query -> [Value]
queryDoc v q = queryFunc q v

queryFunc :: Query -> Value -> [Value]
queryFunc (Query ql) = \cv -> [cv ^. foldr ((.) . toLens) id ql ]
    where
        toLens i = lens (getC i) (setC i)
modifyFunc :: Query -> Value -> Value -> Value
modifyFunc (Query ql) = \cv v -> foldr ((.) .toLens) id ql ^= v $ cv
    where
        toLens i = lens (getC i) (setC i)
