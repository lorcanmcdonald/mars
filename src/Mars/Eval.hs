{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mars.Eval
  ( Output (..),
    run,
  )
where

#if __GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Text.IO (putStrLn)
-- import Mars.Instances ()

import Mars.Command
import Mars.Types
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn)

run :: Command a => MarsState -> a -> IO MarsState
run state = exec . evalCommand state

exec :: (MarsState, Output) -> IO MarsState
exec (newState, Output output) = do
  putStrLn output
  return newState
