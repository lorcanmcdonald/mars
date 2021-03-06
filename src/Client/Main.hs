{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ()
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.IO as TIO
import Mars.Eval
import Mars.Parser
import Mars.Query
import Mars.Types
import Options.Applicative
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.IO as SIO

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

data Mars = Mars {jsonFilename :: String, noninteractive :: Bool}

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info optParser mempty
    optParser =
      Mars
        <$> argument str (metavar "filename")
        <*> switch
          ( short 'n'
              <> long "noninteractive"
              <> help "Force noninteractive mode"
          )

runWithOptions :: Mars -> IO ()
runWithOptions opts = do
  hSetEncoding stdout utf8
  isTTY <- testTTY
  jsonString <- openFileOrStdin . jsonFilename $ opts
  hFlush stdout

  if isTTY && not (noninteractive opts)
    then -- Start an interactive session
    case json2Doc jsonString of
      Just doc -> readEvalPrintLoop $ MarsState {path = DefaultLocation, document = doc}
      Nothing -> TIO.putStrLn "Could not parse JSON document"
    else do
      input <- TIO.hGetContents stdin
      _ <-
        exec
          ( MarsState
              { path = DefaultLocation,
                document = fromJust . json2Doc $ ""
              }
          )
          (Text.lines input)
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
readEvalPrintLoop state =
  bracketOnError
    (initializeInput defaultSettings)
    {- This will only be called if an exception such as a SigINT
     - is received.
     -}
    cancelInput
    (\hd -> loop hd state >> closeInput hd)
  where
    loop :: InputState -> MarsState -> IO ()
    loop hd s = do
      minput <- queryInput hd (getInputLine "> ")
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          s' <- eval s $ Text.pack input
          loop hd s'

eval :: MarsState -> Text.Text -> IO MarsState
eval s input = case parser input of
  Left err -> do
    print err
    return s
  Right [] -> return s
  Right (OpCat x : _) -> run s x
  Right (OpCd x : _) -> run s x
  Right (OpLoad x : _) -> run s x
  Right (OpLs x : _) -> run s x
  Right (OpPwd x : _) -> run s x
  Right (OpSave x : _) -> run s x
  Right (OpSet x : _) -> run s x
