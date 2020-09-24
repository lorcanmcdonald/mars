{-# LANGUAGE DeriveGeneric #-}

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

data Load = Load Text
  deriving (Generic, Show, Eq, Typeable)

instance Command Load where
  readCommand = error "readCommand Load"
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
  renderCommand = error "renderCommand"

instance Arbitrary Load where
  arbitrary = Load <$> arbString

arbString :: Gen Text
arbString = toS <$>
  listOf
    (elements (['A' .. 'Z'] <> ['a' .. 'z']))
    `suchThat` (not . null)
