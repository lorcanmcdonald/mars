{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Mars.Eval
  ( Output (..),
    ansiColor,
    cat,
    cd,
    load,
    ls,
    pwd,
    run,
    save,
    update,
    directoryEntries,
  )
where

#if __GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Strict as Map
import Data.Ix
import Data.Maybe
import Data.String
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO (putStrLn)
import qualified Data.Vector as Vector
import Mars.Command
import Mars.Instances ()
import Mars.Types
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn)

newtype Output = Output Text
  deriving (Show, Eq)

exec :: (MarsState, Output) -> IO MarsState
exec (newState, Output output) = do
  putStrLn output
  return newState

run :: MarsState -> Command -> IO MarsState
run s (Cat queries) = exec $ cat s queries
run s (Cd query) = exec $ cd s query
run s (Load filename) = load s filename
run s (Ls query) = do
  putStrLn . toS . show $ query
  exec $ ls s query
run s (Save filename) = save s filename
run s (Update query value) = exec $ update s query value
run s Pwd = exec $ pwd s

getDocument :: MarsState -> Value
getDocument s = fromMaybe (object []) $ document s

cat :: MarsState -> [Query] -> (MarsState, Output)
cat s [] =
  ( s,
    Output . toS
      . encodePretty
      . queryDoc (path s)
      . getDocument
      $ s
  )
cat s l =
  ( s,
    Output . toS . ByteString.intercalate "\n" $
      (=<<) formattedJSONText l
  )
  where
    formattedJSONText :: Query -> [ByteString.ByteString]
    formattedJSONText q =
      fmap encodePretty
        . queryDoc (path s <> q)
        . getDocument
        $ s

cd :: MarsState -> Query -> (MarsState, Output)
cd s query =
  let newState = s {path = newQuery}
   in (newState, Output "")
  where
    newQuery
      | itemExists = path s
      | otherwise = path s <> query
    itemExists =
      null . queryDoc (path s <> query)
        . getDocument
        $ s

pwd :: MarsState -> (MarsState, Output)
pwd s = (s, Output . renderQuery . path $ s)

update :: MarsState -> Query -> Value -> (MarsState, Output)
update s query value = (s {document = newDoc}, Output "")
  where
    newDoc = doUpdate =<< document s
    doUpdate doc = Just $ modifyDoc doc query value

save :: MarsState -> Text -> IO MarsState
save s filename = s <$ writeFile (Text.unpack filename) jsonString
  where
    jsonString = ByteString.unpack . encodePretty $ toJSON s

load :: MarsState -> Text -> IO MarsState
load s filename = do
  c <- readFile . Text.unpack $ filename
  (loadResult . decode . ByteString.pack) c
  where
    loadResult Nothing = printErr "Could not parse"
    loadResult (Just j) = reportResult . fromJSON $ j
    reportResult (Error err) = printErr err
    reportResult (Success state) = pure state
    printErr err = s <$ hPutStrLn stderr ("Invalid saved state: " <> err)

ls :: MarsState -> Query -> (MarsState, Output)
ls s DefaultLocation = (s, Output . format . list (getDocument s) $ path s)
  where
    format :: [DirectoryEntry] -> Text
    format l =
      Text.intercalate "\n"
        . zipWith
          ansiColor
          (colorMap <$> l)
        $ (\(DirectoryEntry (ItemName name) _) -> name) <$> l
ls s q = (s, Output . format . list (getDocument s) $ path s <> q)
  where
    format :: [DirectoryEntry] -> Text
    format l =
      Text.intercalate "\n"
        . zipWith
          ansiColor
          (colorMap <$> l)
        $ (\(DirectoryEntry (ItemName name) _) -> name) <$> l

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

spread :: (a -> b) -> [a] -> [(a, b)]
spread f a = zip a (f <$> a)

extractSpread :: Functor f => (a, f b) -> f (a, b)
extractSpread (i, l) = (i,) <$> l

spreadMaybe :: (a, Maybe b) -> Maybe (a, b)
spreadMaybe = extractSpread

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

ansiWrap :: forall m. (Monoid m, Data.String.IsString m) => m -> m -> m
ansiWrap colorID text = "\ESC[" <> colorID <> "m" <> text <> "\ESC[0m"

toItemType :: Value -> ItemType
toItemType (Object _) = MarsObject
toItemType (Array _) = MarsList
toItemType (String _) = MarsString
toItemType (Number _) = MarsNumber
toItemType (Bool _) = MarsBool
toItemType Null = MarsNull
