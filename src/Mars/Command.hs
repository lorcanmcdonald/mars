{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Types representing items entered at the Mars command line
module Mars.Command
  ( Command (..),
    globIndices,
    globKeys,
    modifyDoc,
    moveUp,
    normalizeQuery,
    queryDoc,
    renderQuery,
  )
where

import Control.Category
import Data.Aeson
-- import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Functor
import Data.Functor.Identity
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.String.Conv
import Data.Text (Text)
import Data.Vector ((!?), (//))
import Mars.Query
import Mars.Types
import Text.Parsec.Prim (ParsecT)
import Text.Read (readMaybe)
import Prelude hiding ((.), id)

class Command a where
  readCommand :: ParsecT String u Identity a
  evalCommand :: MarsState -> a -> (MarsState, Output)
  printCommand :: a -> (MarsState, Output) -> IO MarsState
  renderCommand :: a -> Text

-- | Output a command in a format similar to how it would have be entered by the
-- user
-- renderCommand :: Command a => a -> Text.Text
-- renderCommand (Cat l) =
--   Text.intercalate " " $ "cat" : (renderQuery <$> l)
-- renderCommand (Ls a) = "ls " <> renderQuery a
-- renderCommand (Save f) = "save " <> f
-- renderCommand (Load f) = "load \"" <> f <> "\""
-- renderCommand (Update q val) =
--   "update "
--     <> renderQuery q
--     <> " "
--     <> Text.pack (ByteString.unpack $ encode val)
-- renderCommand Pwd = "pwd"
-- renderCommand (Cd a) = "cd " <> renderQuery a

-- | A text version of a QueryItem
moveUp :: Query -> Maybe Query
moveUp DefaultLocation = Nothing
moveUp (Query q) = Query <$> (NonEmpty.nonEmpty . NonEmpty.init $ q)

queryDoc :: Query -> Value -> [Value]
queryDoc DefaultLocation v = [v]
queryDoc (Query q) v =
  let x = NonEmpty.head q
      xs = NonEmpty.tail q
      values = findValues x v
   in case NonEmpty.nonEmpty xs of
        Nothing -> concatMap (queryDoc DefaultLocation) values
        Just list -> concatMap (queryDoc (Query list)) values

modifyDoc :: Value -> Query -> Value -> Value
modifyDoc _ DefaultLocation val = val
modifyDoc doc (Query q) val =
  let x = NonEmpty.head q
      xs = NonEmpty.tail q
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

findValues :: QueryItem -> Value -> [Value]
findValues (Glob glob) (Object o) =
  mapMaybe (`Map.lookup` o) $
    globKeys o glob
findValues (Glob glob) (Array a) =
  mapMaybe (a !?) $ globIndices a glob
findValues (Glob _) (String _) = [] :: [Value]
findValues (Glob _) (Number _) = [] :: [Value]
findValues (Glob _) (Bool _) = [] :: [Value]
findValues (Glob _) Null = []
