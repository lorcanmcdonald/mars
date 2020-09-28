{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Types representing items entered at the Mars command line
module Mars.Command
  ( Command (..),
    Action (..),
    globIndices,
    globKeys,
    modifyDoc,
    moveUp,
    normalizeQuery,
    queryDoc,
  )
where

import Control.Category
import Data.Aeson
import Data.Functor
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.String.Conv
import Data.Text (Text)
import Data.Vector ((!?), (//))
import Mars.Query
import Mars.Types
import Text.Read (readMaybe)
import Prelude hiding ((.), id)

class Command a o | a -> o where
  evalCommand :: MarsState -> a -> o

class Action o where
  execCommand :: MarsState -> o -> IO MarsState

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
        . foldr (`Map.insert` v) o
        . globKeys o
        $ glob
    modifyDoc' (Object o) (Glob glob) xs v =
      foldr (applyChanges v (Query . NonEmpty.fromList $ xs)) (Object o)
        . globKeys o
        $ glob
    modifyDoc' (Array a) (Glob glob) [] v =
      Array
        . foldr (\key arr -> arr // [(key, v)]) a
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
