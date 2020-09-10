{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Types representing items entered at the Mars command line
module Mars.Command
  ( globKeys,
    modifyDoc,
    moveUp,
    queryDoc,
    renderCommand,
    renderQuery,
    simplifyQuery,
  )
where

import Control.Category
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Ix
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe
import Data.Monoid
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Mars.Types
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Prelude hiding ((.), id)

-- | Output a command in a format similar to how it would have be entered by the
-- user
renderCommand :: Command -> Text.Text
renderCommand (Cat []) = "cat"
renderCommand (Cat l) =
  "cat "
    <> Text.intercalate " " (renderQuery <$> l)
renderCommand (Ls a) = "ls " <> renderQuery a
renderCommand (Save f) = "save " <> f
renderCommand (Load f) = "load \"" <> f <> "\""
renderCommand (Update q val) =
  "update "
    <> renderQuery q
    <> " "
    <> Text.pack (ByteString.unpack $ encode val)
renderCommand Pwd = "pwd"
renderCommand (Cd a) = "cd " <> renderQuery a

-- | Output a query in a format that would have been entered in the interpreter
renderQuery :: Query -> Text.Text
renderQuery (Query l) = Text.intercalate "/" (renderQueryItem <$> l)

-- | A text version of a QueryItem
renderQueryItem :: QueryItem -> Text.Text
renderQueryItem (Glob g) = mconcat . map renderGlob . toList $ g
renderQueryItem LevelAbove = Text.pack ".."

renderGlob :: GlobItem -> Text.Text
renderGlob AnyChar = "?"
renderGlob AnyCharMultiple = "*"
renderGlob (LiteralString s) = s

moveUp :: Query -> Query
moveUp (Query q) = Query . reverse . drop 1 $ reverse q

simplifyQuery :: Query -> Query
simplifyQuery (Query l) = Query . reverse . foldr simplify [] $ reverse l
  where
    simplify :: QueryItem -> [QueryItem] -> [QueryItem]
    simplify LevelAbove processed = drop 1 processed
    simplify item processed = item : processed

modifyDoc :: Value -> Query -> Value -> Value
modifyDoc v q = set (modifyLens q) v

queryDoc :: Value -> Query -> [Value]
queryDoc v q = queryLens q v

modifyLens :: Query -> Lens' Value Value
modifyLens _ = error "modifyLens"

-- TODO queryLens is not even a Lenslike now
queryLens :: Query -> Value -> [Value]
queryLens (Query []) v = [v]
queryLens (Query (x : xs)) v =
  let values = queryItemLens x v
   in concatMap (queryLens (Query xs)) values

globKeys :: HashMap Text a -> NonEmpty GlobItem -> [Text]
globKeys obj glob = filter (match glob) . Map.keys $ obj

globIndices :: Vector.Vector a -> NonEmpty GlobItem -> [Int]
globIndices vec glob =
  map (read . toS)
    . filter (match glob)
    . map ((toS . show) :: Int -> Text)
    . range
    $ (0, length vec)

match :: NonEmpty GlobItem -> Text -> Bool
match l v = case parse (mkParser l) "" v of
  Left _ -> False
  Right _ -> True
  where
    mkParser :: NonEmpty GlobItem -> Parsec Text u ()
    mkParser = foldr (<*) eof . fmap toParser
    toParser :: GlobItem -> Parsec Text u ()
    toParser (LiteralString s) = () <$ string (toS s)
    toParser AnyChar = () <$ anyToken
    toParser AnyCharMultiple = () <$ many anyToken

queryItemLens :: QueryItem -> Value -> [Value]
queryItemLens item = getter item
  where
    getter :: QueryItem -> Value -> [Value]
    getter LevelAbove _ = error "[5fd31a02] LevelAbove should be optimised out by this point. Need to revisit types"
    getter (Glob glob) (Object o) =
      catMaybes
        . map (`Map.lookup` o)
        $ globKeys o glob
    getter (Glob glob) (Array a) =
      catMaybes . map (a Vector.!?) $ (globIndices a glob)
    getter (Glob _) (String _) = ([] :: [Value])
    getter (Glob _) (Number _) = ([] :: [Value])
    getter (Glob _) (Bool _) = ([] :: [Value])
    getter (Glob _) (Null) = []

-- setter :: QueryItem -> [Value] -> [Value] -> Value
-- setter (Glob glob) v [Array a] =
--   toJSON
--     . map
--       ( \i ->
--           Vector.update a
--             . Vector.fromList
--             $ [(i, v)]
--       )
--     $ globIndices a glob
-- setter (Glob _) v [Object a] = error "[7d743a5d] not implemented"
-- setter (Glob _) _ _ = error "[7d743a5d] Not implemented"
-- setter LevelAbove _ _ = error "[4ac5eac2] Not implemented"
