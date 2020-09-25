{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mars.Command.Load (Load (..)) where

-- import Data.Aeson
-- import Data.String.Conv

import Data.String.Conv
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Mars.Command
import Test.QuickCheck

-- import Text.ParserCombinators.Parsec hiding ((<|>))

newtype Load = Load Text
  deriving (Generic, Show, Eq, Typeable)

instance Command Load where
  evalCommand = error "evalCommand Load"

  -- evalCommand s filename = do
  --   c <- readFile . toS $ filename
  --   (loadResult . decode . toS) c
  --   where
  --     loadResult Nothing = printErr "Could not parse"
  --     loadResult (Just j) = reportResult . fromJSON $ j
  --     reportResult (Error err) = printErr err
  --     reportResult (Success state) = pure state
  --     printErr err = s <$ hPutStrLn stderr ("Invalid saved state: " <> err)
  printCommand = error "printCommand"
  renderCommand (Load f) = "load \"" <> f <> "\""

instance Arbitrary Load where
  arbitrary = Load <$> arbString

arbString :: Gen Text
arbString =
  toS
    <$> listOf
      (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)
