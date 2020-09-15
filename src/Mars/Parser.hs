{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Mars.Parser where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Aeson.Types as AesonTypes
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Char8 as ByteString
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.String.Conv
import qualified Data.Text as Text
import Mars.Command
import Mars.Types
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding ((<|>))

-- | The character used to separate query items when entered on the commandline
querySeparator :: Text.Text
querySeparator = "/"

parser :: Text.Text -> Either ParseError [Command]
parser l = parse commandLine "" $ Text.unpack l

parseQuery :: Text.Text -> Either ParseError Query
parseQuery s = parse query "" $ Text.unpack s

-- | Parse a list of commands
commandLine :: forall u. ParsecT String u Identity [Command]
commandLine = (command `sepBy` char '|') <* eof

command :: forall u. ParsecT String u Identity Command
command = keywordWithArg <|> keyword

keyword :: forall u. ParsecT String u Identity Command
keyword =
  try (Pwd <$ string "pwd")
    <|> try (Cat [] <$ string "cat")
    <|> try (Ls (mempty) <$ string "ls")
    <?> "keyword"

keywordWithArg :: forall u. ParsecT String u Identity Command
keywordWithArg =
  try (Cat <$> (string "cat" *> spaces *> query `sepBy` string " "))
    <|> try (Ls <$> (string "ls" *> spaces *> query))
    <|> try (Save <$> (string "save" *> spaces *> filename))
    <|> try (Load <$> (string "load" *> spaces *> filename))
    <|> try
      ( Update <$> (string "update" *> spaces *> query)
          <*> (spaces *> value)
      )
    <|> try (Cd <$> (string "cd" *> spaces *> query))
    <|> try (Cd (mempty) <$ string "cd")
    <?> "keyword and argument"

queryString :: forall u. ParsecT String u Identity (String, String)
queryString = (,) <$> (string "&" *> noEquals) <*> (string "=" *> noSpaces)

query :: forall u. ParsecT String u Identity Query
query =
  do
    items <- queryItem `sepBy` string (Text.unpack querySeparator)
    return $ case normalizeQuery items of
      Just i -> i
      Nothing -> mempty
    <?> "query"

queryItem :: forall u. ParsecT String u Identity UnnormalizedQueryItem
queryItem =
  try (CurrentLevel <$ string ".")
    <|> (LevelAbove <$ string "..")
    <|> try (GlobInput <$> globItems)
    <?> "queryItem"

namedItem :: forall u. ParsecT String u Identity String
namedItem =
  (many1 . noneOf . fmap (head . Text.unpack) $ [querySeparator, " ", "*", "?"])
    <?> "namedItem"

globItems :: ParsecT String u Identity (NonEmpty GlobItem)
globItems = do
  items <- many1 globItem
  return . fromList $ items

globItem :: ParsecT String u Identity GlobItem
globItem =
  try (AnyChar <$ string "?")
    <|> try (AnyCharMultiple <$ string "*")
    <|> try
      ( do
          str <- many1 . noneOf $ "/ *?"
          return . LiteralString . toS $ str
      )

filename :: forall u. ParsecT String u Identity Text.Text
filename =
  try (Text.pack <$> (string "\"" *> noDoubleQuotes <* string "\""))
    -- Doesn't handle files with spaces ...
    <|> try (Text.pack <$> wspaceSeparated)
    <?> "filename"

noSpaces :: forall u. ParsecT String u Identity String
noSpaces = many1 . noneOf $ " "

noEquals :: forall u. ParsecT String u Identity String
noEquals = many1 . noneOf $ "="

noDoubleQuotes :: forall u. ParsecT String u Identity String
noDoubleQuotes = many1 . noneOf $ "\""

value :: forall u. ParsecT String u Identity AesonTypes.Value
value =
  ( do
      v <- wspaceSeparated
      either
        (const mzero)
        return
        (parseOnly AesonParser.value (ByteString.pack v))
  )
    <?> "simple JSON Value"

wspaceSeparated :: forall u. ParsecT String u Identity String
wspaceSeparated = many1 (noneOf " ") <?> "token"

jsonParser = AesonParser.value
