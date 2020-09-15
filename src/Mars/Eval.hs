{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mars.Eval
  ( run,
    ls,
    cd,
    pwd,
    cat,
    update,
    save,
    load,
    ansiColour,
  )
where

#if __GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.String
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector ((!?))
import Mars.Command
import Mars.Instances ()
import Mars.Types
import System.IO
import Text.Read (readMaybe)

run :: MarsState -> Command -> IO MarsState
run s (Cat queries) = s <$ cat s queries
run s Pwd = s <$ pwd s
run s (Ls query) = s <$ ls s query
run s (Cd query) = cd s query
run s (Update query value) = update s query value
run s (Save filename) = save s filename
run s (Load filename) = load s filename

getDocument :: MarsState -> Value
getDocument s = fromMaybe (object []) $ document s

cat :: MarsState -> [Query] -> IO ()
cat s [] =
  putStrLn . (=<<) (ByteString.unpack . encodePretty)
    . queryDoc (path s)
    . getDocument
    $ s
cat s l =
  putStrLn . ByteString.unpack . ByteString.intercalate "\n" $
    (=<<) formattedJSONText l
  where
    formattedJSONText :: Query -> [ByteString.ByteString]
    formattedJSONText q =
      fmap encodePretty
        . queryDoc (path s <> q)
        . getDocument
        $ s

cd :: MarsState -> Query -> IO MarsState
cd s query = pure s {path = newQuery}
  where
    newQuery
      | itemExists = path s
      | otherwise = path s <> query
    itemExists =
      null . queryDoc (path s <> query)
        . getDocument
        $ s

pwd :: MarsState -> IO ()
pwd = putStrLn . Text.unpack . renderQuery . path

update :: MarsState -> Query -> Value -> IO MarsState
update s query value = return $ s {document = newDoc}
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

ls :: MarsState -> Query -> IO ()
ls s q = putStrLn . Text.unpack . format . list (getDocument s) $ path s <> q
  where
    format :: [Text] -> Text
    format l = Text.intercalate "\n" l
    list :: Value -> Query -> [Text]
    list doc DefaultLocation = list doc (Query (Glob (AnyCharMultiple :| []) :| [])) -- 🤔
    list doc (Query query) =
      zipWith ansiColour colors entries
      where
        entries :: [Text]
        entries = directoryEntries (NonEmpty.head query) doc
        colors :: [ANSIColour]
        colors = map (childColor doc) entries
        childColor :: Value -> Text -> ANSIColour
        childColor (Object obj) key = colourMap . fromMaybe Null $ Map.lookup key obj
        childColor (Array arr) key = colourMap . fromMaybe Null $ do
          k <- readMaybe . toS $ key
          arr !? k
        childColor v _ = colourMap v

colourMap :: Value -> ANSIColour
colourMap (Object _) = Blue
colourMap (Array _) = Blue
colourMap (String _) = Green
colourMap (Number _) = Green
colourMap (Bool _) = Green
colourMap Null = Green

ansiColour :: ANSIColour -> Text -> Text
ansiColour Grey = ansiWrap "30"
ansiColour Red = ansiWrap "31"
ansiColour Green = ansiWrap "32"
ansiColour Yellow = ansiWrap "33"
ansiColour Blue = ansiWrap "34"
ansiColour Magenta = ansiWrap "35"
ansiColour Cyan = ansiWrap "36"
ansiColour White = ansiWrap "37"

ansiWrap :: forall m. (Monoid m, Data.String.IsString m) => m -> m -> m
ansiWrap colourID text = "\ESC[" <> colourID <> "m" <> text <> "\ESC[0m"
