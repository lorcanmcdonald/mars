{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mars.Instances where

import Data.Aeson.TH
  ( constructorTagModifier,
    defaultOptions,
    deriveJSON,
    fieldLabelModifier,
  )
import Mars.Types
import Network.URL

deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''Query
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''QueryItem
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''GlobItem
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''MarsState
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''URL
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''URLType
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''Host
deriveJSON
  defaultOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id
    }
  ''Protocol
