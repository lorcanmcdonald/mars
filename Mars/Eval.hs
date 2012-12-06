{-#LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Mars.Eval
where
import Control.Arrow
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Maybe
import Network.URI (parseURI)
import Network.URL
import Network.HTTP.Types (status200)
import Mars.Command
import Mars.Instances ()
import Mars.Types
import System.IO
import Text.XML.HXT.Core (XmlTree, multi, hasName, ArrowXml, withParseHTML, withWarnings, readString, yes, no, runX, getAttrValue)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Conduit.Browser

run :: State -> Command -> IO State
run s (Cat []) = idempotent s . Prelude.putStrLn . concatMap (ByteString.unpack.encodePretty) . queryDoc (fromMaybe emptyObjectCollection (document s)) $ path s
run s (Cat l)  = idempotent s .
                     Prelude.putStrLn .
                     ByteString.unpack $ ByteString.intercalate "\n" (concatMap formattedJSONText l)
                where
                    formattedJSONText :: Query -> [ByteString.ByteString]
                    formattedJSONText q = map encodePretty .
                         queryDoc (fromMaybe emptyObjectCollection (document s)) $
                         prependToQuery (path s) q
run s (Ls Nothing)                  = idempotent s . printLs s $ path s
run s (Ls (Just query))             = idempotent s . printLs s $ prependToQuery (path s) query
run s (Cd (Query (LevelAbove : _))) = return s {path = moveUp (path s)}
run s (Cd query)                    = return s {path = newQuery' }
        where
            newQuery' = case findItem of
                    [] -> path s
                    _       -> newQuery
            findItem = queryDoc (fromMaybe emptyObjectCollection (document s)) newQuery
            newQuery = prependToQuery (path s) query
run s Href           = idempotent s $ case url s of
                            Nothing -> hPutStrLn stderr "No previous URL"
                            Just u  -> Prelude.putStrLn $ exportURL u
run s Pwd                      = idempotent s . putStrLn . Text.unpack . renderQuery . simplifyQuery $ path s
run s (Login loginPage inputs) = do
                            _ <- loginWithURL s loginPage inputs
                            return s

run s (Get Nothing) = case url s of
                                Nothing -> idempotent s (hPutStrLn stderr "No previous URL")
                                Just u -> getWithURL s u
run s (Get inUrl) = case inUrl of
                            Nothing -> idempotent s (hPutStrLn stderr "Invalid URL")
                            Just u -> getWithURL s u
run s (Update query value) = return s'
                            where
                                newDoc = case document s of
                                            Nothing -> Nothing
                                            Just doc -> Just $ modifyDoc doc query value 
                                s' = s{document = newDoc}
run s (Save filename)              = do
                            writeFile (Text.unpack filename) (ByteString.unpack . encodePretty $ toJSON s)
                            return s
run s (Load filename) = do
                            c <- readFile (Text.unpack filename)
                            case decode (ByteString.pack c) of
                                Nothing -> do
                                            hPutStrLn stderr "Invalid saved state"
                                            return s
                                Just j -> case fromJSON j of
                                            Error err -> idempotent s $ hPutStrLn stderr ("Invalid saved state: " ++ err)
                                            Success state -> return state

idempotent :: State -> IO() -> IO State
idempotent s io = do
                io
                return s

getWithURL :: State -> URL -> IO State
getWithURL s inUrl = case parseURI $ exportURL inUrl of
                Nothing -> do
                    hPutStrLn stderr "Invalid URL"
                    return s
                Just u -> do
                    getURL <- HTTP.parseUrl $ show u
                    rsp  <- HTTP.withManager $ HTTP.httpLbs getURL
                    return s { url = Just inUrl
                             , document = decode $ HTTP.responseBody rsp
                             , path = Query []
                             }

printLs :: State -> Query -> IO()
printLs s q = Prelude.putStrLn . Text.unpack . format $ ls (fromMaybe emptyObjectCollection (document s))  q
    where
        format :: [[Text.Text]] -> Text.Text
        format l = Text.intercalate "\n" $ map (Text.intercalate "\n") l

ls :: Value -> Query -> [[Text.Text]]
ls doc query = map asString elements
            where
                asString :: Value -> [Text.Text]
                asString e = case e of
                        Object o -> zipWith ansiColourText [colourMap $ getChild o k | k <- Map.keys o] $ Map.keys o
                        Array a  -> [ Text.pack $ "Array[" ++ (show.Vector.length) a ++ "]"]
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
        wrap colourID text = "\ESC[" |++| colourID |++| "m" |++| text |++| "\ESC[0m"

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

loginWithURL :: State -> URL -> [(String, String)] -> IO State
loginWithURL s url inputs = login (exportURL url) s

login u s = HTTP.withManager (\manager ->
                        browse manager $ do
                            init_req1 <- HTTP.parseUrl u

                            let req1' = init_req1 { HTTP.method = "POST",
                                                    HTTP.checkStatus = \_ _ -> Nothing }
                            let post_data = []
                            let req1 = HTTP.urlEncodedBody post_data req1'
                            resp1 <- makeRequestLbs req1
                            if HTTP.responseStatus resp1 == status200
                            then
                                do
                                    -- inputNames  <- runX . names $ doc resp1
                                    -- inputValues <- runX . values $ doc resp1
                                    -- action      <- runX . formAction $ doc resp1

                                    cj <- getCookieJar
                                    return s
                            else
                                return s)

                    -- HTTP.withManager $ (\ manager -> do
                    --     browse manager $ do
                    --         getReq <- HTTP.parseUrl . show $ u
                    --         rsp <- makeRequest getReq

                    --         
                    --         
                    --         

                    --         print $ inputs ++ zip inputNames inputValues

                    --         postReq <- HTTP.parseUrl $ "http://localhost/" ++ ( head $ action)
                    --         loginRsp <- makeRequest postReq

                    --         jsonReq <- HTTP.parseUrl $ "http://localhost/sla/"
                    --         jsonRsp <- makeRequest jsonReq

                    --         print jsonRsp

                    --         return s { url = Just inUrl
                    --              , document = decode $ HTTP.responseBody rsp
                    --              , path = Query []
                    --              , cookies = cookies
                    --              })
                where
                    names tree = tree >>> css "input" >>> getAttrValue "name"
                    values tree = tree >>> css "input" >>> getAttrValue "value"
                    formAction tree = tree >>> css "form" >>> getAttrValue "action"
                    doc rsp = readString [withParseHTML yes, withWarnings no] . ByteString.unpack . HTTP.responseBody $ rsp
