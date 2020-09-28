{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}

module Mars.Eval (run) where

import Mars.Command
import Mars.Types
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn)

run :: ((Command a o), (Action o)) => MarsState -> a -> IO MarsState
run state = execCommand state . evalCommand state
