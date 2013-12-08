{-#LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Mars.Eval
where
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Maybe
import Data.Monoid
import Mars.Command
import Mars.Instances ()
import Mars.Types
import System.IO
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

run :: MarsState -> Command -> IO MarsState
run s (Cat []) = idempotent s . Prelude.putStrLn . (=<<) (ByteString.unpack.encodePretty) . queryDoc (fromMaybe emptyObjectCollection (document s)) $ path s
run s (Cat l)  = idempotent s .
                     Prelude.putStrLn .
                     ByteString.unpack $ ByteString.intercalate "\n" ((=<<) formattedJSONText l)
                where
                    formattedJSONText :: Query -> [ByteString.ByteString]
                    formattedJSONText q = fmap encodePretty .
                         queryDoc (fromMaybe emptyObjectCollection (document s)) $
                         path s <> q

run s (Ls query)             = idempotent s . printLs s $ path s <> query

run s (Cd (Query (LevelAbove : _))) = return s {path = moveUp (path s)}
run s (Cd query)                    = return s {path = newQuery' }
        where
            newQuery' = case findItem of
                    [] -> path s
                    _  -> newQuery
            findItem = queryDoc (fromMaybe emptyObjectCollection (document s)) newQuery
            newQuery = path s <> query

run s Pwd                      = idempotent s . putStrLn . Text.unpack . renderQuery . simplifyQuery $ path s

run s (Update query value) = return s'
                            where
                                newDoc = case document s of
                                            Nothing -> Nothing
                                            Just doc -> Just $ modifyDoc doc query value
                                s' = s{document = newDoc}

run s (Save filename) = do
                            writeFile (Text.unpack filename) (ByteString.unpack . encodePretty $ toJSON s)
                            return s

run s (Load filename) = do
                            c <- readFile (Text.unpack filename)
                            case decode (ByteString.pack c) of
                                Nothing -> do
                                            hPutStrLn stderr "Invalid saved state"
                                            return s
                                Just j -> case fromJSON j of
                                            Error err -> idempotent s $ hPutStrLn stderr ("Invalid saved state: " <> err)
                                            Success state -> return state

idempotent :: MarsState -> IO() -> IO MarsState
idempotent s io = do
                io
                return s

printLs :: MarsState -> Query -> IO()
printLs s q = Prelude.putStrLn . Text.unpack . format $ ls (fromMaybe emptyObjectCollection (document s))  q
    where
        format :: [[Text.Text]] -> Text.Text
        format l = Text.intercalate "\n" (Text.intercalate "\n" <$> l)

ls :: Value -> Query -> [[Text.Text]]
ls doc query = asString <$> elements
            where
                asString :: Value -> [Text.Text]
                asString e = case e of
                        Object o -> zipWith ansiColourText [colourMap $ getChild o k | k <- Map.keys o] $ Map.keys o
                        Array a  -> [ Text.pack $ "Array[" <> (show.Vector.length) a <> "]"]
                        _        -> []
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

data ANSIColour = Grey| Red | Green | Yellow | Blue | Magenta| Cyan | White

ansiColourText :: ANSIColour -> Text.Text -> Text.Text
ansiColourText color t = case color of
                            Grey    -> wrap "30" t
                            Red     -> wrap "31" t
                            Green   -> wrap "32" t
                            Yellow  -> wrap "33" t
                            Blue    -> wrap "34" t
                            Magenta -> wrap "35" t
                            Cyan    -> wrap "36" t
                            White   -> wrap "37" t
    where
        wrap colourID text = "\ESC[" <> colourID <> "m" <> text <> "\ESC[0m"
