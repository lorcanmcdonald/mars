{-# LANGUAGE OverloadedStrings, CPP #-}
module Main
where
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Aeson
import Mars.Eval
import Mars.Parser
import Mars.Types
import Options.Applicative
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Control.Exception
import System.IO as SIO
import Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text

#ifdef WINDOWS
readline :: String -> IO (Maybe String)
readline prompt = do
                SIO.putStr prompt
                hFlush stdout
                line <- SIO.getLine
                return $ Just line

addHistory :: String -> IO ()
addHistory _ = return ()

testTTY :: IO Bool
testTTY = return True
#endif
#ifndef WINDOWS
testTTY :: IO Bool
testTTY = hIsTerminalDevice stdin
#endif

data Mars = Mars { jsonFilename :: String, noninteractive :: Bool }

-- | The initial state
initialState :: MarsState
initialState = MarsState { path        = Query []
                         , document    = Nothing
                         }

main :: IO ()
main = execParser opts >>= runWithOptions
    where
        opts = info optParser mempty
        optParser = Mars
                <$> argument str (metavar "filename")
                <*> switch (short 'n'
                        <> long "noninteractive"
                        <> help "Force noninteractive mode")

runWithOptions :: Mars -> IO ()
runWithOptions opts = do
    hSetEncoding stdout utf8
    isTTY <- testTTY
    jsonString <- openFileOrStdin . jsonFilename $ opts
    hFlush stdout

    if isTTY && not (noninteractive opts)
        then -- Start an interactive session
            readEvalPrintLoop $ initialState { document = json2Doc jsonString }

        else
            do
                input <- TIO.hGetContents stdin
                _ <- exec initialState (Text.lines input)
                return ()
    where
        json2Doc :: ByteString.ByteString -> Maybe Value
        json2Doc = decode

openFileOrStdin :: String -> IO ByteString.ByteString
openFileOrStdin fname = do
    fileHandle <- openFile fname ReadMode
    contents <- SIO.hGetContents fileHandle
    return . ByteString.pack $ contents

exec :: MarsState -> [Text.Text] -> IO MarsState
exec = foldM eval

readEvalPrintLoop :: MarsState -> IO ()
readEvalPrintLoop state = bracketOnError (initializeInput defaultSettings)
            cancelInput -- This will only be called if an exception such as a SigINT is received.
            (\ hd -> loop hd state >> closeInput hd)
    where
        loop :: InputState -> MarsState -> IO ()
        loop hd state = do
            minput <- queryInput hd (getInputLine "> ")
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do
                                state' <- eval state $ Text.pack input
                                loop hd state'

eval :: MarsState -> Text.Text -> IO MarsState
eval s input = case parser input of
            Left err -> do
                        print err
                        return s
            Right [] -> return s
            Right (x : _) -> run s x
