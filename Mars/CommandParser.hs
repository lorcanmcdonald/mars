{-#LANGUAGE OverloadedStrings #-}
module Mars.CommandParser where
import Control.Monad
import Data.Attoparsec (parseOnly)
import Network.URL
import Mars.Types
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import qualified Data.Aeson.Parser as AesonParser
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
commandLine = sepBy command (char '|')

command = keywordWithArg <|> keyword

keyword = (do
            _ <- string "href"
            return Href)
        <|> (do
            _ <- string "pwd"
            return Pwd)
        <|> try (do
            _ <- string "cat"
            return $ Cat Nothing)
        <|> try (do
            _ <- string "get"
            return $ Get Nothing)
        <|> try (do
            _ <- string "ls"
            return $ Ls Nothing)
        <?> "keyword"

keywordWithArg = try (do
            _ <- string "get"
            _ <- spaces
            u <- uri
            return $ Get u)
        <|> try (do
            _ <- string "cat"
            _ <- spaces
            q <- maybeQuery
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

uri = (do
        s <- many1 (noneOf " ")
        return $ importURL s)
        <?> "uri"

maybeQuery = try (do
                    q <- query
                    return $ Just q)
        <|> return Nothing
        <?> "optional query"

query = (do
        items <- queryItem `sepBy` string (Text.unpack querySeparator)
        case items of
            [] -> mzero
            _  -> return $ Query items)
        <?> "query"

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
                item <- matchQuery
                return $ NamedItem $ Text.pack item)
        <?> "queryItem"
            where
                matchQuery = many1 alphaNum

filename = (do
        f <- wspaceSeparated -- Doesn't handle files with spaces of course...
        return $ Text.pack f)
        <?> "filename"

value = (do
        v <- wspaceSeparated
        case parseOnly AesonParser.value (ByteString.pack v) of
            Left _ -> mzero
            Right val -> return val)
        <?> "simple JSON Value"

wspaceSeparated = many1 (noneOf " ") <?> "token"
