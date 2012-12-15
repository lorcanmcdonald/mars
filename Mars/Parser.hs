{-#LANGUAGE OverloadedStrings, RankNTypes#-}
module Mars.Parser where
import Control.Applicative
import Control.Monad
import Network.URL
import Mars.Types
import Data.Attoparsec.Text 
import qualified Data.Attoparsec as AP
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

-- | The character used to separate query items when entered on the commandline
querySeparator :: Text.Text
querySeparator = "/"

-- parser :: Text.Text -> Either ParseError [ Command ]
parser :: Text.Text -> Either Text.Text [Command]
parser l = case parseOnly commandLine l of
                Left str -> Left . Text.pack $ str
                Right r -> Right r

-- parseQuery :: Text.Text -> Either ParseError Query
parseQuery :: Text.Text -> Either Text.Text Query
parseQuery s = case parseOnly query s of
                Left str -> Left . Text.pack $ str
                Right r -> Right r

-- | Parse a list of commands
commandLine :: Parser [Command]
commandLine = many1 command

command :: Parser Command
command = keywordWithArg <|> keyword

keyword :: Parser Command
keyword = try (do
            _ <- string "href"
            return Href)
        <|> try (do
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
            return $ Ls (Query []))
        <?> "keyword"

keywordWithArg :: Parser Command
keywordWithArg = try (do
            _ <- string "get"
            _ <- skipSpace
            u <- optionalURI
            return $ Get u)
        <|> try (do
            _ <- string "login"
            _ <- skipSpace
            u <- uri
            _ <- skipSpace
            q <- many queryString
            return $ Login u q)
        <|> try (do
            _ <- string "cat"
            _ <- skipSpace
            q <- query `sepBy` string " "
            return $ Cat q)
        <|> try (do
            _ <- string "ls"
            _ <- skipSpace
            q <- query
            return $ Ls q)
        <|> try (do
            _ <- string "save"
            _ <- skipSpace 
            f <- filename
            return $ Save f)
        <|> try (do
            _ <- string "load"
            _ <- skipSpace 
            f <- filename
            return $ Load f)
        <|> try(do
            _ <- string "update"
            _ <- skipSpace
            q <- query
            _ <- skipSpace
            v <- value
            return $ Update q v)
        <|> try (do
            _ <- string "cd"
            _ <- skipSpace
            q <- query
            return $ Cd q)
        <|> try (do
            _ <- string "cd"
            return $ Cd (Query []) )
        <?> "keyword and argument"

queryString :: Parser (String, String)
queryString = do
            _ <- string "&"
            k <- many1 (notChar '=')
            _ <- string "="
            v<- many1 (notChar ' ')
            return (k, v)

uri :: Parser URL
uri = (do
        s <- many1 (notChar ' ')
        case importURL s of
                Nothing -> mzero
                Just u -> return u
        )
        <?> "URI"

optionalURI :: Parser (Maybe URL)
optionalURI = (do
        s <- many1 (notChar ' ')
        return $ importURL s)
        <?> "optional URI"

maybeQuery :: Parser (Maybe Query)
maybeQuery = try (do
                    q <- query
                    return $ Just q)
        <|> return Nothing
        <?> "optional query"

query :: Parser Query
query = (do
        items <- queryItem `sepBy` string querySeparator
        case items of
            [] -> mzero
            _  -> return $ Query items)
        <?> "query"

queryItem :: Parser QueryItem
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
                item <- many1 . notChar $ '"'
                _ <- string "\""
                return . NamedItem . Text.pack $ item)
            <|> try (do
                item <- namedItem
                return . NamedItem . Text.pack $ item)
        <?> "queryItem"

namedItem :: Parser String
namedItem = (many1 . notChar . Text.head $ querySeparator)
        <?> "namedItem"

filename :: Parser Text.Text
filename = try (do
                _ <- string "\""
                f <- many1 . notChar $ '"'
                _ <- string "\""
                return . Text.pack $ f)
            <|> try (do
                f <- wspaceSeparated -- Doesn't handle files with spaces of course...
                return $ Text.pack f)
            <?> "filename"

value :: Parser AesonTypes.Value
value = (do
        v <- wspaceSeparated
        case AP.parseOnly AesonParser.value (ByteString.pack v) of
            Left _ -> mzero
            Right val -> return val)
        <?> "simple JSON Value"

wspaceSeparated :: Parser String
wspaceSeparated = many1 (notChar ' ') <?> "token"
