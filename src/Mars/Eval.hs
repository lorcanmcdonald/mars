{-# LANGUAGE OverloadedStrings, DoAndIfThenElse, RankNTypes #-}
module Mars.Eval
(run, ls, cd, pwd, cat, update, save, load)
where
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Maybe
import Data.Monoid
import Data.String
import Mars.Command
import Mars.Instances ()
import Mars.Types
import System.IO
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

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
cat s [] = putStrLn . (=<<) (ByteString.unpack . encodePretty) . queryDoc (getDocument s) $ path s
cat s l = putStrLn . ByteString.unpack . ByteString.intercalate "\n" $ (=<<) formattedJSONText l
    where
        formattedJSONText :: Query -> [ByteString.ByteString]
        formattedJSONText q = fmap encodePretty .
             queryDoc (getDocument s) $ path s <> q

cd :: MarsState -> Query -> IO MarsState
cd s (Query (LevelAbove : _)) = pure s { path = moveUp (path s) }
cd s query                    = pure s { path = newQuery }
    where
        newQuery | itemExists = path s
                 | otherwise  = path s <> query
        itemExists = null . queryDoc (getDocument s) $ (path s <> query)

pwd :: MarsState -> IO ()
pwd s = putStrLn . Text.unpack . renderQuery . simplifyQuery $ path s

update :: MarsState -> Query -> Value -> IO MarsState
update s query value = return $ s { document = newDoc }
    where
        newDoc = maybe Nothing (\ doc -> Just $ modifyDoc doc query value) (document s)

save :: MarsState -> Text.Text -> IO MarsState
save s filename = s <$ writeFile (Text.unpack filename) jsonString
    where
        jsonString = ByteString.unpack . encodePretty $ toJSON s

load :: MarsState -> Text.Text -> IO MarsState
load s filename = do
        c <- readFile . Text.unpack $ filename
        (loadResult . decode . ByteString.pack) c
    where
        loadResult Nothing = s <$ hPutStrLn stderr "Invalid saved state"
        loadResult (Just j) = reportReult . fromJSON $ j

        reportReult (Error err) = s <$ hPutStrLn stderr ("Invalid saved state: " <> err)
        reportReult (Success state) = pure state

ls :: MarsState -> Query -> IO ()
ls s q = putStrLn . Text.unpack . format . list (getDocument s) $ path s <> q
    where
        format :: [[Text.Text]] -> Text.Text
        format l = Text.intercalate "\n" (Text.intercalate "\n" <$> l)

        list :: Value -> Query -> [[Text.Text]]
        list doc query = asString <$> elements
            where
                asString :: Value -> [Text.Text]
                asString (Object o) = zipWith ansiColourText colorChildren $ Map.keys o
                    where
                        colorChildren = [ colourMap $ getChild o k | k <- Map.keys o ]

                asString (Array a)  = [ Text.pack $ "Array[" <> (show . Vector.length) a <> "]"]
                asString _          = []

                elements :: [Value]
                elements = queryDoc doc query

                getChild :: Map.HashMap Text.Text Value -> Text.Text -> Value
                getChild obj key = fromMaybe emptyObject $ Map.lookup key obj

                colourMap :: Value -> ANSIColour
                colourMap (Object _) = Blue
                colourMap (Array _)  = Blue
                colourMap (String _) = Green
                colourMap (Number _) = Green
                colourMap (Bool _)   = Green
                colourMap (Null)     = Green

data ANSIColour = Grey | Red | Green | Yellow | Blue | Magenta | Cyan | White

ansiColourText :: ANSIColour -> Text.Text -> Text.Text
ansiColourText Grey    = ansiWrap "30"
ansiColourText Red     = ansiWrap "31"
ansiColourText Green   = ansiWrap "32"
ansiColourText Yellow  = ansiWrap "33"
ansiColourText Blue    = ansiWrap "34"
ansiColourText Magenta = ansiWrap "35"
ansiColourText Cyan    = ansiWrap "36"
ansiColourText White   = ansiWrap "37"

ansiWrap :: forall m . (Monoid m, Data.String.IsString m) => m -> m -> m
ansiWrap colourID text = "\ESC[" <> colourID <> "m" <> text <> "\ESC[0m"
