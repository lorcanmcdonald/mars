{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Mars.Parser where
import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString (parseOnly)
import Mars.Types
import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.Parsec.Prim (ParsecT)
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text

-- | The character used to separate query items when entered on the commandline
querySeparator :: Text.Text
querySeparator = "/"

parser :: Text.Text -> Either ParseError [ Command ]
parser l = parse commandLine "" $ Text.unpack l

parseQuery :: Text.Text -> Either ParseError Query
parseQuery s = parse query "" $ Text.unpack s

-- | Parse a list of commands
commandLine :: forall u . ParsecT String u Identity [Command]
commandLine = sepBy command (char '|')

command :: forall u . ParsecT String u Identity Command
command = keywordWithArg <|> keyword

keyword :: forall u . ParsecT String u Identity Command
keyword = try (Pwd <$ string "pwd")
        <|> try (Cat [] <$ string "cat")
        <|> try (Ls (Query []) <$ string "ls")
        <?> "keyword"

keywordWithArg :: forall u . ParsecT String u Identity Command
keywordWithArg =
        try (Cat <$> (string "cat" *> spaces *> query `sepBy` string " "))
        <|> try (Ls <$> (string "ls" *> spaces *> query))
        <|> try (Save <$> (string "save" *> spaces *> filename))
        <|> try (Load <$> (string "load" *> spaces *> filename))
        <|> try (Update <$> (string "update" *> spaces *> query)
                        <*> (spaces *> value))
        <|> try (Cd <$> (string "cd" *> spaces *> query))
        <|> try ( Cd (Query []) <$ string "cd")
        <?> "keyword and argument"

queryString :: forall u . ParsecT String u Identity (String, String)
queryString = (,) <$> (string "&" *> noEquals) <*> (string "=" *> noSpaces)

query :: forall u . ParsecT String u Identity Query
query = do
        items <- queryItem `sepBy` string (Text.unpack querySeparator)
        returnQuery items
        <?> "query"
    where
        returnQuery [] = mzero
        returnQuery items = return $ Query items

queryItem :: forall u . ParsecT String u Identity QueryItem
queryItem = try (LevelAbove <$ string "..")
            <|> try (WildCardItem <$ string "*")
            <|> try (IndexedItem . read <$> many1 digit)
            <|> try (NamedItem . Text.pack <$>
                (string "\"" *> noDoubleQuotes <* string "\""))
            <|> try (NamedItem . Text.pack <$> namedItem)
        <?> "queryItem"

namedItem :: forall u . ParsecT String u Identity String
namedItem = (many1 . noneOf . fmap (head . Text.unpack) $ [querySeparator, " "])

        <?> "namedItem"

filename :: forall u . ParsecT String u Identity Text.Text
filename = try ( Text.pack <$> (string "\"" *> noDoubleQuotes <* string "\""))
            -- Doesn't handle files with spaces ...
            <|> try ( Text.pack <$> wspaceSeparated)
            <?> "filename"

noSpaces :: forall u . ParsecT String u Identity [Char]
noSpaces = many1 . noneOf $ " "

noEquals :: forall u . ParsecT String u Identity [Char]
noEquals = many1 . noneOf $ "="

noDoubleQuotes :: forall u . ParsecT String u Identity [Char]
noDoubleQuotes = many1 . noneOf $ "\""

value :: forall u . ParsecT String u Identity AesonTypes.Value
value = (do
        v <- wspaceSeparated
        either
            (const mzero)
            return ( parseOnly AesonParser.value (ByteString.pack v)))
        <?> "simple JSON Value"

wspaceSeparated :: forall u . ParsecT String u Identity String
wspaceSeparated = many1 (noneOf " ") <?> "token"

jsonParser = AesonParser.value
