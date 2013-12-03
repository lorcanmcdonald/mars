{-#LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Mars.Instances where
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier, constructorTagModifier)
import Network.URL
import Mars.Types

deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''Query
deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''QueryItem
deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''MarsState
deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''URL
deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''URLType
deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''Host
deriveJSON defaultOptions{fieldLabelModifier = id, constructorTagModifier = id} ''Protocol
