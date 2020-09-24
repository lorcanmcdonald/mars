{-# LANGUAGE DeriveGeneric #-}

module Mars.Command.Save (Save (..)) where

-- import Data.String.Conv

-- import Data.Functor.Identity

import Data.String.Conv
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Mars.Command
import Test.QuickCheck

-- import Mars.Types
-- import Text.Parsec.Prim (ParsecT)
-- import Text.ParserCombinators.Parsec hiding ((<|>))

data Save = Save Text
  deriving (Generic, Show, Eq, Typeable)

instance Command Save where
  readCommand = error "readCommand Save"
  evalCommand = error "evalCommand Save"

  -- evalCommand s filename = s <$ writeFile (toS filename) jsonString
  --   where
  --     jsonString = toS . encodePretty $ toJSON s
  printCommand = error "printCommand"
  renderCommand = error "renderCommand"

instance Arbitrary Save where
  arbitrary = Save <$> arbString

arbString :: Gen Text
arbString =
  toS <$>
    listOf
      (elements (['A' .. 'Z'] <> ['a' .. 'z']))
      `suchThat` (not . null)
