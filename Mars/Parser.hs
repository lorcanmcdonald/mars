{-#LANGUAGE OverloadedStrings, RankNTypes#-}
module Mars.Parser where
import Control.Applicative
import Control.Monad
-- import Data.Attoparsec (parseOnly)
import Network.URL
import Mars.Types
import Data.Functor.Identity
-- import Text.ParserCombinators.Parsec
-- import Text.Parsec.Prim (ParsecT)
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
parser l = case parse commandLine l of
            Partial f -> case f "" of 
                Fail _ _ reason -> Left . Text.pack $ reason
                Done _ result -> Right result

-- parseQuery :: Text.Text -> Either ParseError Query
parseQuery :: Text.Text -> Either Text.Text Query
parseQuery s = case parse query s of
                Partial f -> case f "" of
                    Fail _ _ reason -> Left . Text.pack $ reason
                    Done _ result -> Right result

-- | Parse a list of commands
-- commandLine :: forall u. ParsecT String u Identity [Command]
commandLine = sepBy command (char '|')

-- command :: forall u. ParsecT String u Identity Command
command = keywordWithArg <|> keyword

-- keyword :: forall u. ParsecT String u Identity Command
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

-- keywordWithArg :: forall u. ParsecT String u Identity Command
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

-- queryString :: forall u. ParsecT String u Identity (String, String)
queryString = do
            _ <- string "&"
            k <- many1 (notChar '=')
            _ <- string "="
            v<- many1 (notChar ' ')
            return (k, v)

-- uri :: forall u. ParsecT String u Identity URL
uri = (do
        s <- many1 (notChar ' ')
        case importURL s of
                Nothing -> mzero
                Just u -> return u
        )
        <?> "URI"
-- optionalURI :: forall u. ParsecT String u Identity (Maybe URL)
optionalURI = (do
        s <- many1 (notChar ' ')
        return $ importURL s)
        <?> "optional URI"

-- maybeQuery :: forall u. ParsecT String u Identity (Maybe Query)
maybeQuery = try (do
                    q <- query
                    return $ Just q)
        <|> return Nothing
        <?> "optional query"

-- query :: forall u. ParsecT String u Identity Query
query = (do
        items <- queryItem `sepBy` string querySeparator
        case items of
            [] -> mzero
            _  -> return $ Query items)
        <?> "query"

-- queryItem :: forall u. ParsecT String u Identity QueryItem
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
                return . NamedItem $ Text.pack item)
            <|> try (do
                item <- namedItem
                return . NamedItem $ Text.pack item)
        <?> "queryItem"

-- namedItem :: forall u. ParsecT String u Identity String
namedItem = (many1 . notChar . Text.head $ querySeparator)
        <?> "namedItem"

-- filename :: forall u. ParsecT String u Identity Text.Text
filename = try (do
                _ <- string "\""
                f <- many1 . notChar $ '"'
                _ <- string "\""
                return . Text.pack $ f)
            <|> try (do
                f <- wspaceSeparated -- Doesn't handle files with spaces of course...
                return $ Text.pack f)
            <?> "filename"

-- value :: forall u.  ParsecT String u Identity AesonTypes.Value
value = (do
        v <- wspaceSeparated
        case AP.parseOnly AesonParser.value (ByteString.pack v) of
            Left _ -> mzero
            Right val -> return val)
        <?> "simple JSON Value"

-- wspaceSeparated :: forall u. ParsecT String u Identity String
wspaceSeparated = many1 (notChar ' ') <?> "token"
