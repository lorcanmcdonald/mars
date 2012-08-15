{-#LANGUAGE OverloadedStrings #-}
module Main
where
import Control.Monad
import Mars.Command
import Mars.Parser
import Mars.Types
import Mars.CommandParser
import System.Console.Readline
import System.IO
import Data.Text.IO as TIO
import qualified Data.Text as Text

main :: IO()
main = do
    isTTY <- hIsTerminalDevice stdin
    if isTTY
        then -- Start an interactive session
            readEvalPrintLoop initialState
        else -- Read from stdin
            do
                input <- TIO.hGetContents stdin
                _ <- exec initialState (Text.lines input)
                return ()

exec :: State -> [Text.Text] -> IO State
exec = foldM eval'

readEvalPrintLoop :: State -> IO ()
readEvalPrintLoop state = do
    maybeLine <- readline "> "
    case maybeLine of
        Nothing     -> return ()
        Just line   -> do
                addHistory line
                state' <- eval' state $ Text.pack line
                readEvalPrintLoop state'

eval' :: State -> Text.Text -> IO State
eval' s input = case parser input of
            Left err -> do
                        print err
                        return s
            Right [] -> return s
            Right (x:_) -> run s x
