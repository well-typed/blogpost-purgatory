module C.Color.Native (
    Color(..)
  , red
  ) where

#include "rust_wrapper.h"

import Codec.Borsh
import Generics.SOP qualified as SOP
import GHC.Generics qualified as GHC

import Data.Annotated
import Data.Structured qualified
import Foreign.Rust.Marshall.Fixed

data Color = Color { r :: Double, g :: Double, b :: Double }
  deriving stock (Show, GHC.Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving anyclass (Data.Structured.Show)
  deriving CanAnnotate via NoAnnotation Color
  deriving (BorshSize, ToBorsh, FromBorsh) via AsStruct Color

{# fun pure unsafe rust_wrapper_red as red
     { allocFixedBuffer- `Color'& fromBorsh*
     }
  -> `()'
#}

