{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Mars.Parser where

import Control.Applicative
import Control.Monad
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Aeson.Types as AesonTypes
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Char8 as ByteString
import Data.Functor.Identity
import Data.String.Conv
import qualified Data.Text as Text
import Mars.Command.Cat
import Mars.Command.Cd
import Mars.Command.Load
import Mars.Command.Ls
import Mars.Command.Pwd
import Mars.Command.Save
import Mars.Command.Set
import Mars.Query (query, querySeparator)
import Test.QuickCheck
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding ((<|>))

data Operation
  = OpCat Cat
  | OpCd Cd
  | OpLoad Load
  | OpLs Ls
  | OpPwd Pwd
  | OpSave Save
  | OpSet Set
  deriving (Show, Eq)

instance Arbitrary Operation where
  arbitrary =
    oneof
      [ OpCat <$> arbitrary,
        OpCd <$> arbitrary,
        OpLoad <$> arbitrary,
        OpLs <$> arbitrary,
        OpPwd <$> arbitrary,
        OpSave <$> arbitrary,
        OpSet <$> arbitrary
      ]

parser :: Text.Text -> Either ParseError [Operation]
parser l = parse commandLine "" $ toS l

-- | Parse a list of commands
commandLine :: forall u. ParsecT String u Identity [Operation]
commandLine = (command `sepBy` char '|') <* eof

command :: forall u. ParsecT String u Identity Operation
command =
  try (OpLs . Ls <$> (string "ls" *> spaces *> query))
    <|> try (OpPwd Pwd <$ string "pwd")
    <|> try (OpCat . Cat <$> (string "cat" *> space *> spaces *> (query `sepBy1` (string " " *> spaces))))
    <|> try (OpCat . Cat <$> ([] <$ string "cat"))
    <|> try (OpSave . Save <$> (string "save" *> spaces *> filename))
    <|> try (OpLoad . Load <$> (string "load" *> spaces *> filename))
    <|> try
      ( OpSet
          <$> ( Set <$> (string "set" *> spaces *> query)
                  <*> (spaces *> value)
              )
      )
    <|> ( try (OpCd . Cd <$> (string "cd" *> spaces *> query))
            <|> try ((OpCd . Cd) mempty <$ string "cd")
        )

queryString :: forall u. ParsecT String u Identity (String, String)
queryString = (,) <$> (string "&" *> noEquals) <*> (string "=" *> noSpaces)

namedItem :: forall u. ParsecT String u Identity String
namedItem =
  (many1 . noneOf . fmap (head . Text.unpack) $ [querySeparator, " ", "*", "?"])
    <?> "namedItem"

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
