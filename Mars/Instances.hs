{-#LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mars.Instances where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Network.URL
import Mars.Command
import Mars.Parser
import Mars.Types
import Text.ParserCombinators.Parsec (parse)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text

instance FromJSON Query where
    parseJSON (String s) = case parse query "(from json)" $ Text.unpack s of
                            Left _ -> mzero
                            Right q -> return q
    parseJSON _          = mzero

instance FromJSON CollectionValue where
    parseJSON (Object o) = return $ O o
    parseJSON (Array a)  = return $ A a
    parseJSON _          = mzero

instance ToJSON CollectionValue where
    toJSON (O o) = Object o
    toJSON (A a) = Array a

instance FromJSON State where
    parseJSON (Object v) = State <$>
                                v .: "url" <*>
                                v .: "path" <*>
                                v .: "document"
    parseJSON _          = mzero

instance FromJSON URL where
    parseJSON (String s) = case importURL (Text.unpack s) of
                            Nothing -> mzero
                            Just u -> return u
    parseJSON _          = mzero


instance ToJSON State where
    toJSON s = object [ "url" .= u
                      , "path" .= q
                      , "document" .= d
                      ]
            where
                u = case url s of
                    Nothing -> Null
                    Just aURL -> String $ Text.pack $ exportURL aURL
                q = renderQuery $ path s
                d = case document s of
                    Nothing -> Null
                    Just doc -> String $ Text.pack $ ByteString.unpack $ encode doc

