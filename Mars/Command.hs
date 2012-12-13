-- |Types representing items entered at the Mars command line
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Mars.Command

where

import Control.Applicative
import Control.Category
import Data.Aeson
import Data.HashMap.Strict (keys, elems, insert)
import qualified Data.HashMap.Strict as Map
import Data.Lens.Common
import Data.Maybe
import Data.Monoid
import Mars.Types
import Network.URL
import Prelude hiding (id, (.))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Conduit as HTTP

-- | The initial state
initialState :: State
initialState = State { url         = Nothing
                     , path        = Query []
                     , document    = Nothing
                     , cookies     = HTTP.def
                     }

-- |Output a command in a format similar to how it would have be entered by the user
renderCommand :: Command -> Text.Text
renderCommand (Get Nothing)      = "get"
renderCommand (Get (Just u))     = "get "    <> Text.pack ( exportURL u)
renderCommand (Cat [])           = "cat"
renderCommand (Cat l)            = "cat "    <> Text.intercalate " " (renderQuery <$> l)
renderCommand (Ls a)             = "ls "     <> renderQuery a
renderCommand (Save f)           = "save "   <> f
renderCommand (Load f)           = "load \"" <> f <> "\""
renderCommand (Update q val)     = "update " <> renderQuery q <> " "  <> Text.pack ( ByteString.unpack $ encode val)
renderCommand (Login url inputs) = "login "  
                                                <> (Text.pack . show $ url) 
                                                <> (Text.intercalate "&" $ (\ (f, s) -> Text.pack $ f <> "=" <> s) <$> inputs)
renderCommand Href               = "href"
renderCommand Pwd                = "pwd"
renderCommand (Cd a)             = "cd "     <> renderQuery a

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
setC (IndexedItem i) v (Array a) = toJSON . Vector.update a . Vector.fromList $ [(i, v)]
setC (NamedItem n) v (Object o)  = toJSON $ insert n v o
setC WildCardItem v (Array a)    = Array $ Vector.map (\ _ -> v)  a
setC WildCardItem v (Object a)   = Object $ Map.map (\ _ -> v)  a
setC _ _ c                       = c

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

allQueries :: Value -> [Query]
allQueries (Array a)    = prependToAll ((\ n -> Query [IndexedItem n ]) <$> [0 .. Vector.length a]) (concat $ allQueries <$> Vector.toList a)
allQueries (Object o)   = prependToAll ((\ n -> Query [NamedItem n ]) <$> keys o) (concat $ allQueries <$> elems o)
allQueries _            = []

prependToAll :: [Query] -> [Query] -> [Query]
prependToAll heads tails = [ h <> t | h <- heads, t <- tails ] 
