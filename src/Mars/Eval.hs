{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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
import Mars.Types
import Mars.Command
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn)

run :: Command a => MarsState -> a -> IO MarsState
run state command = exec . evalCommand state $ command

exec :: (MarsState, Output) -> IO MarsState
exec (newState, Output output) = do
  putStrLn output
  return newState

-- run s (Cd query) = exec $ cd s query
-- run s (Load filename) = load s filename
-- run s (Ls query) = do
--   putStrLn . toS . show $ query
--   exec $ ls s query
-- run s (Save filename) = save s filename
-- run s (Update query value) = exec $ update s query value
-- run s Pwd = exec $ pwd s

-- getDocument :: MarsState -> Value
-- getDocument s = fromMaybe (object []) $ document s
