{-#LANGUAGE OverloadedStrings, RankNTypes#-}
module Mars.Parser where
import Control.Monad
import Data.Attoparsec (parseOnly)
import Network.URL
import Mars.Types
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

-- | The character used to separate query items when entered on the commandline
querySeparator :: Text.Text
querySeparator = "/"

parser :: Text.Text -> Either ParseError [ Command ]
parser l = parse commandLine "(test)" $ Text.unpack l

parseQuery :: Text.Text -> Either ParseError Query
parseQuery s = parse query "(unit-test)" $ Text.unpack s

-- | Parse a list of commands
commandLine :: forall u. ParsecT String u Identity [Command]
commandLine = sepBy command (char '|')

command :: forall u. ParsecT String u Identity Command
command = keywordWithArg <|> keyword

keyword :: forall u. ParsecT String u Identity Command
keyword = (do
            _ <- string "href"
            return Href)
        <|> (do
            _ <- string "pwd"
            return Pwd)
        <|> try (do
            _ <- string "cat"
            return $ Cat [])
        <|> try (do
            _ <- string "get"
            return $ Get Nothing)
        <|> try (do
            _ <- string "ls"
            return $ Ls Nothing)
        <?> "keyword"

keywordWithArg :: forall u. ParsecT String u Identity Command
keywordWithArg = try (do
            _ <- string "get"
            _ <- spaces
            u <- optionalURI
            return $ Get u)
        <|> try (do
            _ <- string "login"
            _ <- spaces
            u <- uri
            _ <- spaces
            q <- many queryString
            return $ Login u q)
        <|> try (do
            _ <- string "cat"
            _ <- spaces
            q <- query `sepBy` string " "
            return $ Cat q)
        <|> try (do
            _ <- string "ls"
            _ <- spaces
            q <- maybeQuery
            return $ Ls q)
        <|> try (do
            _ <- string "save"
            _ <- spaces 
            f <- filename
            return $ Save f)
        <|> try (do
            _ <- string "load"
            _ <- spaces 
            f <- filename
            return $ Load f)
        <|> try(do
            _ <- string "update"
            _ <- spaces
            q <- query
            _ <- spaces
            v <- value
            return $ Update q v)
        <|> try (do
            _ <- string "cd"
            _ <- spaces
            q <- query
            return $ Cd q)
        <|> try (do
            _ <- string "cd"
            return $ Cd (Query []) )
        <?> "keyword and argument"

queryString :: forall u. ParsecT String u Identity (String, String)
queryString = (do
            _ <- string "&"
            k <- many1 (noneOf "=")
            _ <- string "="
            v<- many1 (noneOf " ")
            return (k, v))

uri :: forall u. ParsecT String u Identity URL
uri = (do
        s <- many1 (noneOf " ")
        case importURL s of
                Nothing -> mzero
                Just u -> return u
        )
        <?> "URI"
optionalURI :: forall u. ParsecT String u Identity (Maybe URL)
optionalURI = (do
        s <- many1 (noneOf " ")
        return $ importURL s)
        <?> "optional URI"

maybeQuery :: forall u. ParsecT String u Identity (Maybe Query)
maybeQuery = try (do
                    q <- query
                    return $ Just q)
        <|> return Nothing
        <?> "optional query"

query :: forall u. ParsecT String u Identity Query
query = (do
        items <- queryItem `sepBy` string (Text.unpack querySeparator)
        case items of
            [] -> mzero
            _  -> return $ Query items)
        <?> "query"

queryItem :: forall u. ParsecT String u Identity QueryItem
queryItem = try (do
                _ <- string ".."
                return LevelAbove)
            <|> try (do
                _ <- string "*"
                return WildCardItem)
            <|> try (do
                item <- many1 digit
                return $ IndexedItem (read item))
            <|> try (do
                _ <- string "\""
                item <- many1 . noneOf $ "\""
                _ <- string "\""
                return $ NamedItem $ Text.pack item)
            <|> try (do
                item <- namedItem
                return $ NamedItem $ Text.pack item)
        <?> "queryItem"

namedItem :: forall u. ParsecT String u Identity String
namedItem = (many1 $ noneOf $ map (head . Text.unpack) [querySeparator, " "])
        <?> "namedItem"

filename :: forall u. ParsecT String u Identity Text.Text
filename = try (do
                _ <- string "\""
                f <- many1 . noneOf $ "\""
                _ <- string "\""
                return . Text.pack $ f)
            <|> try (do
                f <- wspaceSeparated -- Doesn't handle files with spaces of course...
                return $ Text.pack f)
            <?> "filename"

value :: forall u.  ParsecT String u Identity AesonTypes.Value
value = (do
        v <- wspaceSeparated
        case parseOnly AesonParser.value (ByteString.pack v) of
            Left _ -> mzero
            Right val -> return val)
        <?> "simple JSON Value"

wspaceSeparated :: forall u. ParsecT String u Identity String
wspaceSeparated = many1 (noneOf " ") <?> "token"
