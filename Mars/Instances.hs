{-#LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Mars.Instances where
import Data.Aeson.TH (deriveJSON)
import Network.URL
import Mars.Types
import qualified Network.HTTP.Conduit as HTTP

deriveJSON id ''Query
deriveJSON id ''QueryItem
deriveJSON id ''CollectionValue
deriveJSON id ''State
deriveJSON id ''URL
deriveJSON id ''URLType
deriveJSON id ''Host
deriveJSON id ''Protocol
deriveJSON id ''HTTP.CookieJar
deriveJSON id ''HTTP.Cookie
