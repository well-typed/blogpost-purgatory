module C.Color.Foreign (
    Color(..)
  , red
  , rustWrapperColorFromJSON
  , rustWrapperColorToJSON
  ) where

#include "rust_wrapper.h"

import Codec.Borsh
import Data.FixedSizeArray (FixedSizeArray)
import Data.Text (Text)
import Data.Word

import Foreign.Rust.External.JSON (JSON)
import Foreign.Rust.Marshall.Fixed
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw

newtype Color = Color (FixedSizeArray 24 Word8)
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)

{# fun pure unsafe rust_wrapper_red as red
     { allocFixedBuffer- `Color'& fromBorsh*
     }
  -> `()'
#}

{# fun unsafe rust_wrapper_color_from_json as rustWrapperColorFromJSON
     { toBorshVar*  `JSON'&
     , getVarBuffer `Buffer (Either Text Color)'&
     }
  -> `()'
#}

{# fun unsafe rust_wrapper_color_to_json as rustWrapperColorToJSON
     { toBorshFixed* `Color'&
     , getVarBuffer  `Buffer JSON'&
     }
  -> `()'
#}



