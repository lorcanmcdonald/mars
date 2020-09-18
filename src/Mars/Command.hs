{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Types representing items entered at the Mars command line
module Mars.Command
  ( globIndices,
    globKeys,
    modifyDoc,
    moveUp,
    normalizeQuery,
    queryDoc,
    renderCommand,
    renderQuery,
    renderUnnormalizedQueryItem,
  )
where

import Control.Category
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Ix
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Monoid
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!?), (//), Vector)
import Mars.Types
import Text.Parsec (Parsec)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Read (readMaybe)
import Prelude hiding ((.), id)

-- | Output a command in a format similar to how it would have be entered by the
-- user
renderCommand :: Command -> Text.Text
renderCommand (Cat l) =
  Text.intercalate " " $ "cat" : (renderQuery <$> l)
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
renderQuery DefaultLocation = "/"
renderQuery (Query l) =
  Text.intercalate "/"
    . NonEmpty.toList
    $ (renderQueryItem <$> l)

-- | A text version of a QueryItem
renderUnnormalizedQueryItem :: UnnormalizedQueryItem -> Text.Text
renderUnnormalizedQueryItem (GlobInput g) = mconcat . map renderGlob . toList $ g
renderUnnormalizedQueryItem LevelAbove = Text.pack ".."
renderUnnormalizedQueryItem CurrentLevel = Text.pack "."

renderQueryItem :: QueryItem -> Text.Text
renderQueryItem (Glob g) = mconcat . map renderGlob . toList $ g

renderGlob :: GlobItem -> Text.Text
renderGlob AnyChar = "?"
renderGlob AnyCharMultiple = "*"
renderGlob (LiteralString s) = s

moveUp :: Query -> Maybe Query
moveUp DefaultLocation = Nothing
moveUp (Query q) = Query <$> (NonEmpty.nonEmpty . NonEmpty.init $ q)

normalizeQuery :: [UnnormalizedQueryItem] -> Maybe Query
normalizeQuery l =
  Query
    <$> ( NonEmpty.nonEmpty
            . reverse
            . fmap unsafeToQI
            . foldr simplify []
            . reverse
            $ l
        )
  where
    unsafeToQI :: UnnormalizedQueryItem -> QueryItem
    unsafeToQI (GlobInput g) = Glob g
    unsafeToQI i =
      error $
        "`normalizeQuery` did not remove an "
          <> show i
          <> ". This is a bug in `mars`"
    simplify :: UnnormalizedQueryItem -> [UnnormalizedQueryItem] -> [UnnormalizedQueryItem]
    simplify CurrentLevel processed = processed
    simplify LevelAbove processed = drop 1 processed
    simplify item processed = item : processed

queryDoc :: Query -> Value -> [Value]
queryDoc DefaultLocation v = [v]
queryDoc (Query q) v =
  let x = NonEmpty.head q
      xs = NonEmpty.tail q
      values = queryItemLens x v
   in case NonEmpty.nonEmpty xs of
        Nothing -> concatMap (queryDoc DefaultLocation) values
        Just list -> concatMap (queryDoc (Query list)) values

modifyDoc :: Value -> Query -> Value -> Value
modifyDoc _ DefaultLocation val = val
modifyDoc doc (Query query) val =
  let x = NonEmpty.head query
      xs = NonEmpty.tail query
   in modifyDoc' doc x xs val
  where
    modifyDoc' :: Value -> QueryItem -> [QueryItem] -> Value -> Value
    modifyDoc' (Object o) (Glob glob) [] v =
      Object
        . foldr
          (`Map.insert` v)
          o
        . globKeys o
        $ glob
    modifyDoc' (Object o) (Glob glob) xs v =
      foldr (applyChanges v (Query . NonEmpty.fromList $ xs)) (Object o)
        . globKeys o
        $ glob
    modifyDoc' (Array a) (Glob glob) [] v =
      Array
        . foldr
          (\key arr -> arr // [(key, v)])
          a
        . globIndices a
        $ glob
    modifyDoc' (Array a) (Glob glob) xs v =
      foldr (applyChanges v (Query . NonEmpty.fromList $xs)) (Array a)
        . fmap (toS . show)
        . globIndices a
        $ glob
    modifyDoc' (String _) _ _ v = v
    modifyDoc' (Number _) _ _ v = v
    modifyDoc' (Bool _) _ _ v = v
    modifyDoc' Null _ _ v = v

applyChanges :: Value -> Query -> Text -> Value -> Value
applyChanges v xs key (Object obj) = modifyDoc (fromJust . Map.lookup key $ obj) xs v
applyChanges v xs key (Array arr) =
  case readMaybe . toS $ key of
    Just k -> modifyDoc (fromJust $ arr !? k) xs v
    Nothing -> Array arr
applyChanges _ _ _ s@(String _) = s
applyChanges _ _ _ s@(Number _) = s
applyChanges _ _ _ s@(Bool _) = s
applyChanges _ _ _ s@Null = s

globKeys :: HashMap Text a -> NonEmpty GlobItem -> [Text]
globKeys obj glob = filter (match glob) . Map.keys $ obj

globIndices :: Vector a -> NonEmpty GlobItem -> [Int]
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
queryItemLens (Glob glob) (Object o) =
  mapMaybe (`Map.lookup` o) $
    globKeys o glob
queryItemLens (Glob glob) (Array a) =
  mapMaybe (a !?) $ globIndices a glob
queryItemLens (Glob _) (String _) = [] :: [Value]
queryItemLens (Glob _) (Number _) = [] :: [Value]
queryItemLens (Glob _) (Bool _) = [] :: [Value]
queryItemLens (Glob _) Null = []

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
