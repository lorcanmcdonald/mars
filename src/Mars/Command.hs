{-# LANGUAGE OverloadedStrings, Rank2Types #-}
-- |Types representing items entered at the Mars command line
module Mars.Command
(renderCommand
, renderQuery
, queryDoc
, modifyDoc
, moveUp
, simplifyQuery)

where

import Control.Applicative
import Control.Category
import Data.Aeson
import Data.HashMap.Strict (insert)
import qualified Data.HashMap.Strict as Map
import Control.Lens
import Data.Maybe
import Data.Monoid
import Mars.Types
import Prelude hiding (id, (.))
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Vector as Vector

{- |Output a command in a format similar to how it would have be entered by the
user -}
renderCommand :: Command -> Text.Text
renderCommand (Cat []) = "cat"
renderCommand (Cat l) = "cat "
                       <> Text.intercalate " " (renderQuery <$> l)
renderCommand (Ls a) = "ls " <> renderQuery a
renderCommand (Save f) = "save " <> f
renderCommand (Load f) = "load \"" <> f <> "\""
renderCommand (Update q val) = "update "
                             <> renderQuery q
                             <> " "
                             <> Text.pack ( ByteString.unpack $ encode val)
renderCommand Pwd = "pwd"
renderCommand (Cd a) = "cd " <> renderQuery a

-- |Output a query in a format that would have been entered in the interpreter
renderQuery :: Query -> Text.Text
renderQuery (Query l) = Text.intercalate "/" (renderQueryItem <$> l)

-- |A text version of a QueryItem
renderQueryItem :: QueryItem -> Text.Text
renderQueryItem (NamedItem n) = n
renderQueryItem (IndexedItem i) = Text.pack (show i)
renderQueryItem WildCardItem = Text.pack "*"
renderQueryItem LevelAbove = Text.pack ".."

moveUp :: Query -> Query
moveUp (Query q) = Query . reverse . drop 1 $ reverse q

simplifyQuery :: Query -> Query
simplifyQuery (Query l) = Query . reverse . foldr simplify [] $ reverse l
    where
        simplify :: QueryItem -> [QueryItem] -> [QueryItem]
        simplify (LevelAbove) processed = drop 1 processed
        simplify item processed = item : processed

modifyDoc :: Value -> Query -> Value -> Value
modifyDoc v q = set (queryLens q) v

queryDoc :: Value -> Query -> [Value]
queryDoc v q = [ v ^. queryLens q ]

queryLens :: Query -> Lens' Value Value
queryLens (Query items) = foldr ((.) . queryItemLens) id items

queryItemLens :: QueryItem -> Lens' Value Value
queryItemLens item = lens (getter item) (setter item)
    where
        getter :: QueryItem -> Value -> Value
        getter (IndexedItem i) (Array a) = fromMaybe (object [])
                                         $ (Vector.!?) a i
        getter (NamedItem n) (Object o) = fromMaybe (object [])
                                        $ Map.lookup n o
        getter _ _ = object []

        setter :: QueryItem -> Value -> Value -> Value
        setter (IndexedItem i) v (Array a) = toJSON
            . Vector.update a . Vector.fromList $ [(i, v)]
        setter (NamedItem n) v (Object o) = toJSON $ insert n v o
        setter WildCardItem v (Array a) = Array $ Vector.map (const v) a
        setter WildCardItem v (Object a) = Object $ Map.map (const v) a
        setter _ _ c = c
