{-#LANGUAGE OverloadedStrings, CPP #-}
module Main
where
import Control.Monad
import Mars.Command
import Mars.Eval
import Mars.Types
import Mars.Parser
import Network (withSocketsDo)
#ifndef WINDOWS
import System.Console.Readline
#endif
import System.IO as SIO
import Data.Text.IO as TIO
import qualified Data.Text as Text

#ifdef WINDOWS
readline :: String -> IO(Maybe String)
readline prompt = do
                SIO.putStr prompt
                hFlush stdout
                line <- SIO.getLine
                return $ Just line

addHistory :: String -> IO()
addHistory _ = return ()

testTTY :: IO Bool
testTTY = return True
#endif
#ifndef WINDOWS
testTTY = hIsTerminalDevice stdin
#endif

main :: IO()
main =  do
    hSetEncoding stdout utf8
    isTTY <- testTTY
    hFlush stdout
    -- withSocketsDo $ readEvalPrintLoop initialState
    if isTTY
        then -- Start an interactive session
            withSocketsDo $ readEvalPrintLoop initialState
        else -- Read from stdin
            do
                input <- TIO.hGetContents stdin
                _ <- exec initialState (Text.lines input)
                return ()

exec :: State -> [Text.Text] -> IO State
exec = foldM eval

readEvalPrintLoop :: State -> IO ()
readEvalPrintLoop state = do
    maybeLine <- readline "> "
    hFlush stdout
    case maybeLine of
        Nothing     -> return ()
        Just line   -> do
                addHistory line
                state' <- eval state $ Text.pack line
                readEvalPrintLoop state'

eval :: State -> Text.Text -> IO State
eval s input = case parser input of
            Left err -> do
                        print err
                        return s
            Right [] -> return s
            Right (x:_) -> do
                        -- print x
                        run s x
