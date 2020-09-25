{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Mars.Command.Ls (Ls (..), ansiColor) where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Ix
import Data.Maybe
import Data.String
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (putStrLn)
import Data.Typeable
import qualified Data.Vector as Vector
import GHC.Generics
import Mars.Command
import Mars.Query (Query (..))
import Mars.Types
import Test.QuickCheck
import Prelude hiding (putStrLn)

newtype Ls = Ls Query
  deriving (Generic, Show, Eq, Typeable)

instance Command Ls where
  evalCommand s (Ls DefaultLocation) = (s, Output . format . list (document s) $ path s)
    where
      format :: [DirectoryEntry] -> Text
      format l =
        Text.intercalate "\n"
          . zipWith
            ansiColor
            (colorMap <$> l)
          $ (\(DirectoryEntry (ItemName name) _) -> name) <$> l
  evalCommand s (Ls q) = (s, Output . format . list (document s) $ path s <> q)
    where
      format :: [DirectoryEntry] -> Text
      format l =
        Text.intercalate "\n"
          . zipWith
            ansiColor
            (colorMap <$> l)
          $ (\(DirectoryEntry (ItemName name) _) -> name) <$> l
  printCommand _ (state, Output o) = do
    putStrLn o
    return state
  renderCommand (Ls a) = "ls " <> renderQuery a

list :: Value -> Query -> [DirectoryEntry]
list doc query =
  concatMap directoryEntries . queryDoc query $ doc

directoryEntries :: Value -> [DirectoryEntry]
directoryEntries (Object o) =
  let toDirectoryEntry :: (Text, Value) -> DirectoryEntry
      toDirectoryEntry (name, v) =
        DirectoryEntry
          (ItemName . toS . show $ name)
          (toItemType v)
   in toDirectoryEntry
        <$> catMaybes
          ( spreadMaybe
              <$> spread (o Map.!?) (Map.keys o)
          )
directoryEntries (Array o) =
  let toDirectoryEntry :: (Int, Value) -> DirectoryEntry
      toDirectoryEntry (name, v) =
        DirectoryEntry
          (ItemName . toS . show $ name)
          (toItemType v)
   in toDirectoryEntry
        <$> catMaybes
          ( spreadMaybe
              <$> spread (o Vector.!?) ((\x -> range (0, length x)) o)
          )
directoryEntries (String _) = []
directoryEntries (Number _) = []
directoryEntries (Bool _) = []
directoryEntries Null = []

colorMap :: DirectoryEntry -> ANSIColour
colorMap (DirectoryEntry _ MarsObject) = Blue
colorMap (DirectoryEntry _ MarsList) = Blue
colorMap (DirectoryEntry _ MarsString) = Green
colorMap (DirectoryEntry _ MarsNumber) = Green
colorMap (DirectoryEntry _ MarsBool) = Green
colorMap (DirectoryEntry _ MarsNull) = Green

ansiColor :: ANSIColour -> Text -> Text
ansiColor Grey = ansiWrap "30"
ansiColor Red = ansiWrap "31"
ansiColor Green = ansiWrap "32"
ansiColor Yellow = ansiWrap "33"
ansiColor Blue = ansiWrap "34"
ansiColor Magenta = ansiWrap "35"
ansiColor Cyan = ansiWrap "36"
ansiColor White = ansiWrap "37"

ansiWrap :: (Monoid m, Data.String.IsString m) => m -> m -> m
ansiWrap colorID text = "\ESC[" <> colorID <> "m" <> text <> "\ESC[0m"

spread :: (a -> b) -> [a] -> [(a, b)]
spread f a = zip a (f <$> a)

extractSpread :: Functor f => (a, f b) -> f (a, b)
extractSpread (i, l) = (i,) <$> l

spreadMaybe :: (a, Maybe b) -> Maybe (a, b)
spreadMaybe = extractSpread

toItemType :: Value -> ItemType
toItemType (Object _) = MarsObject
toItemType (Array _) = MarsList
toItemType (String _) = MarsString
toItemType (Number _) = MarsNumber
toItemType (Bool _) = MarsBool
toItemType Null = MarsNull

instance Arbitrary Ls where
  arbitrary = Ls <$> arbitrary
