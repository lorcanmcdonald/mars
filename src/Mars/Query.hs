{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Query
  ( GlobItem (..),
    Query (..),
    QueryItem (..),
    normalizeQuery,
    querySeparator,
    parseQuery, -- TODO these parsers could be part of a `Parsable` typeclass perhaps?
    query,
    queryItem,
    globItems,
    globItem,
    globKeys,
    globIndices,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson hiding ((<?>))
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Ix
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import GHC.Generics
import Mars.Renderable
import Test.QuickCheck
import qualified Test.QuickCheck.Modifiers as Modifiers
import Text.Parsec (Parsec)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding ((<|>))

-- | The datatype representing the queries possible for commands that select
--  - items
--  -
data Query
  = DefaultLocation
  | Query (NonEmpty QueryItem)
  deriving (Generic, Show, Eq)

instance Semigroup Query where
  DefaultLocation <> b = b
  Query _ <> DefaultLocation = DefaultLocation
  (Query a) <> (Query b) = Query (a <> b)

instance Monoid Query where
  mempty = DefaultLocation

instance Arbitrary Query where
  arbitrary = Query . NonEmpty.fromList . Modifiers.getNonEmpty <$> arbitrary

instance FromJSON Query

instance ToJSON Query

newtype QueryItem
  = Glob (NonEmpty GlobItem)
  deriving (Generic, Show, Eq)

instance Arbitrary QueryItem where
  arbitrary =
    oneof
      [ Glob <$> genGlob
      ]

instance FromJSON QueryItem

instance ToJSON QueryItem

genGlob :: Gen (NonEmpty GlobItem)
genGlob = do
  startGlob <- Modifiers.getNonEmpty <$> arbitrary
  specialGlob <- oneof [pure AnyChar, pure AnyCharMultiple]
  endGlob <- Modifiers.getNonEmpty <$> arbitrary
  return . NonEmpty.fromList $ (startGlob <> [specialGlob] <> endGlob)

data UnnormalizedQueryItem
  = GlobInput (NonEmpty GlobItem)
  | LevelAbove
  | CurrentLevel
  deriving (Generic, Show, Eq)

data GlobItem
  = AnyChar
  | AnyCharMultiple
  | LiteralString Text
  deriving (Generic, Show, Eq)

instance Arbitrary GlobItem where
  arbitrary =
    oneof [pure AnyChar, pure AnyCharMultiple]

instance FromJSON GlobItem

instance ToJSON GlobItem

-- | The character used to separate query items when entered on the commandline
querySeparator :: Text.Text
querySeparator = "/"

instance Renderable Query where
  render DefaultLocation = "/"
  render (Query l) =
    Text.intercalate querySeparator
      . toList
      $ (render <$> l)

instance Renderable UnnormalizedQueryItem where
  render (GlobInput g) = mconcat . map renderGlob . toList $ g
  render LevelAbove = Text.pack ".."
  render CurrentLevel = Text.pack "."

instance Renderable QueryItem where
  render (Glob g) = mconcat . toList $ renderGlob <$> g

renderGlob :: GlobItem -> Text.Text
renderGlob AnyChar = "?"
renderGlob AnyCharMultiple = "*"
renderGlob (LiteralString s) = s

normalizeQuery :: [UnnormalizedQueryItem] -> Maybe Query
normalizeQuery l =
  Query
    <$> ( NonEmpty.nonEmpty
            . reverse
            . fmap unsafeToQI
            . foldr simplify []
            . reverse
            $ l
        )
  where
    unsafeToQI :: UnnormalizedQueryItem -> QueryItem
    unsafeToQI (GlobInput g) = Glob g
    unsafeToQI i =
      error $
        "`normalizeQuery` did not remove an "
          <> show i
          <> ". This is a bug in `mars`"
    simplify :: UnnormalizedQueryItem -> [UnnormalizedQueryItem] -> [UnnormalizedQueryItem]
    simplify CurrentLevel processed = processed
    simplify LevelAbove processed = drop 1 processed
    simplify item processed = item : processed

parseQuery :: Text.Text -> Either ParseError Query
parseQuery s = parse query "" $ Text.unpack s

query :: ParsecT String u Identity Query
query =
  do
    items <- queryItem `sepBy` string (Text.unpack querySeparator)
    return $ fromMaybe mempty . normalizeQuery $ items
    <?> "query"

queryItem :: ParsecT String u Identity UnnormalizedQueryItem
queryItem =
  try (CurrentLevel <$ string ".")
    <|> (LevelAbove <$ string "..")
    <|> try (GlobInput <$> globItems)
    <?> "queryItem"

globItems :: ParsecT String u Identity (NonEmpty GlobItem)
globItems = do
  items <- many1 globItem
  return . NonEmpty.fromList $ items

globItem :: ParsecT String u Identity GlobItem
globItem =
  try (AnyChar <$ string "?")
    <|> try (AnyCharMultiple <$ string "*")
    <|> try
      ( do
          str <- many1 . noneOf $ "/ *?"
          return . LiteralString . toS $ str
      )

globKeys :: HashMap Text a -> NonEmpty GlobItem -> [Text]
globKeys obj glob = filter (match glob) . Map.keys $ obj

globIndices :: Vector a -> NonEmpty GlobItem -> [Int]
globIndices vec glob =
  map (read . toS)
    . filter (match glob)
    . map ((toS . show) :: Int -> Text)
    . range
    $ (0, length vec)

match :: NonEmpty GlobItem -> Text -> Bool
match l v = case parse (mkParser l) "" v of
  Left _ -> False
  Right _ -> True
  where
    mkParser :: NonEmpty GlobItem -> Parsec Text u ()
    mkParser = foldr (<*) eof . fmap toParser
    toParser :: GlobItem -> Parsec Text u ()
    toParser (LiteralString s) = () <$ string (toS s)
    toParser AnyChar = () <$ anyToken
    toParser AnyCharMultiple = () <$ many anyToken
