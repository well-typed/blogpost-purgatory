{-# OPTIONS_GHC -Wno-orphans #-}
module Color.Foreign (
    Color -- opaque
  , red
  , colorToJSON
  , colorFromJSON
  ) where

import Data.Aeson qualified as Aeson
import Data.Bifunctor
import GHC.Stack

import Data.Structured qualified
import Foreign.Rust.External.JSON (JSON, UseExternalJSON)
import Foreign.Rust.External.JSON qualified as External
import Foreign.Rust.Failure
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.JSON

import C.Color.Foreign

colorToJSON :: Color -> JSON
colorToJSON = withPureBorshVarBuffer . rustWrapperColorToJSON

colorFromJSON :: HasCallStack => JSON -> Either Failure Color
colorFromJSON = first mkFailure . withPureBorshVarBuffer . rustWrapperColorFromJSON

instance External.ToJSON Color where
  toJSON = colorToJSON

instance External.FromJSON Color where
  fromJSON = colorFromJSON

deriving via UseExternalJSON Color instance Aeson.ToJSON   Color
deriving via UseExternalJSON Color instance Aeson.FromJSON Color

deriving via AsJSON Color instance Show                 Color
deriving via AsJSON Color instance Data.Structured.Show Color
