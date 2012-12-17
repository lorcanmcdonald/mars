{-#LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
module Mars.Eval
where
import Control.Arrow
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Generics.Aliases
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Network.URI (parseURI)
import Network.URL
-- import Network.HTTP.Types (status200)
import Mars.Command
import Mars.Instances ()
import Mars.Types
import System.IO
import Text.XML.HXT.Core (XmlTree, multi, hasName, ArrowXml, withParseHTML, withWarnings, readString, yes, no, runX, getAttrValue)
import qualified Data.ByteString.Char8 as OtherByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Conduit.Browser

run :: State -> Command -> IO State
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
                    _       -> newQuery
            findItem = queryDoc (fromMaybe emptyObjectCollection (document s)) newQuery
            newQuery = path s <> query

run s Href           = idempotent s $ case url s of
                            Nothing -> hPutStrLn stderr "No previous URL"
                            Just u  -> Prelude.putStrLn $ exportURL u

run s Pwd                      = idempotent s . putStrLn . Text.unpack . renderQuery . simplifyQuery $ path s

run s (Login loginPage inputList) = do
                            s' <- loginWithURL s loginPage inputList
                            -- print res
                            print "Ok"
                            return s'

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

css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

loginWithURL :: State -> URL -> [(String, String)] -> IO State
loginWithURL s u overrides = do
                            (resp1, cj) <- HTTP.withManager (\manager -> browse manager $ do
                                init_req1   <- HTTP.parseUrl . exportURL $ u
                                response <- makeRequestLbs $ post init_req1 []
                                sessionCookies <- getCookieJar
                                return (response, sessionCookies))

                            time <- getCurrentTime
                            formDetails <- getFormDetails resp1
                            combinedInputs <- return . map toByteStringPair .filter notDefined $ [(k, lookup k overrides `orElse` (lookup k $ inputs formDetails))
                                                                 | k <- fst <$> inputs formDetails]

                            print formDetails
                            login_req  <- HTTP.parseUrl . ("http://localhost/" ++ ). head . formActions $ formDetails
                            (login_req', _) <- return $ HTTP.insertCookiesIntoRequest login_req cj time
                            -- cookies  <- getCookieJar
                            finalCookies <- HTTP.withManager (\manager -> browse manager $ do
                                _ <- makeRequestLbs $ post login_req' combinedInputs
                                getCookieJar)

                            return s{cookies = finalCookies}
        where
            post :: Monad m => HTTP.Request m -> [(OtherByteString.ByteString, OtherByteString.ByteString)] -> HTTP.Request m
            post req formData = HTTP.urlEncodedBody formData (req { HTTP.method = "POST", HTTP.checkStatus = \_ _ -> Nothing })

            notDefined :: (a, Maybe b) -> Bool
            notDefined (_, Just a) = True
            notDefined (_, Nothing) = False

            toByteStringPair :: (String, Maybe String) -> (OtherByteString.ByteString, OtherByteString.ByteString)
            toByteStringPair (a, Just b) = (OtherByteString.pack a, OtherByteString.pack b)
            toByteStringPair (a, Nothing) = (OtherByteString.pack a,"")


getFormDetails :: HTTP.Response ByteString.ByteString -> IO FormDetails
getFormDetails resp = do
    iNames  <- runX . names . doc $ resp
    iValues <- runX . values . doc $ resp
    actions  <- runX . formAction . doc $ resp
    return FormDetails { inputs  = zip iNames iValues
                         , formActions = actions
                         }
    where
        names           :: ArrowXml cat => cat a XmlTree -> cat a String
        names tree      = tree >>> css "input" >>> getAttrValue "name"
        values          :: ArrowXml cat => cat a XmlTree -> cat a String
        values tree     = tree >>> css "input" >>> getAttrValue "value"
        formAction      :: ArrowXml cat => cat a XmlTree -> cat a String
        formAction tree = tree >>> css "form"  >>> getAttrValue "action"
        doc rsp         = readString [withParseHTML yes, withWarnings no] . ByteString.unpack . HTTP.responseBody $ rsp

data FormDetails = FormDetails { inputs  :: [(String, String)]
                               , formActions  :: [String] }
                    deriving (Show)

