module C.Certificate (
    Certificate(..)
  , SecretKey(..)
  , rustWrapperSelfSigned
  , rustWrapperCertificateSubject
  , exampleKey
  , rustWrapperToPem
  , toPemExternal
  , fromPem
  ) where

#include "rust_wrapper.h"

import Codec.Borsh
import Data.ByteString qualified as Strict
import Data.FixedSizeArray (FixedSizeArray)
import Data.String
import Data.Text (Text)
import Data.Word

import Data.Structured qualified
import Foreign.Rust.Marshall.External
import Foreign.Rust.Marshall.Fixed
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw
import Foreign.Rust.Serialisation.Raw.Base64

newtype Certificate = Certificate Strict.ByteString
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)
  deriving (Show, Data.Structured.Show, IsString) via AsBase64 Certificate

newtype SecretKey = SecretKey (FixedSizeArray 32 Word8)
  deriving newtype (BorshSize, BorshMaxSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)
  deriving (Show, Data.Structured.Show, IsString) via AsBase64 SecretKey

{# fun unsafe rust_wrapper_generate_simple_self_signed as rustWrapperSelfSigned
     { toBorshVar*  `[Text]'&
     , getVarBuffer `Buffer (Certificate, SecretKey)'&
     }
  -> `()'
#}

{# fun unsafe rust_wrapper_get_certificate_subject as rustWrapperCertificateSubject
     { toBorshVar*  `Certificate'&
     , getVarBuffer `Buffer Text'&
     }
  -> `()'
#}

{# fun pure unsafe rust_wrapper_example_key as exampleKey
     {                   `Word64'
     , allocFixedBuffer- `SecretKey'& fromBorsh*
     }
  -> `()'
#}

{# fun unsafe rust_wrapper_key_to_pem as rustWrapperToPem
     { toBorshFixed* `SecretKey'&
     , getVarBuffer  `Buffer Text'&
     }
  -> `()'
#}

{# fun pure unsafe rust_wrapper_key_to_pem_external as toPemExternal
     { toBorshFixed* `SecretKey'&
     }
  -> `Text' fromExternalBorsh*
#}

{# fun pure unsafe rust_wrapper_key_from_pem as fromPem
     { toBorshVar*     `Text'&
     , allocMaxBuffer- `Maybe SecretKey'& fromBorsh*
     }
  -> `()'
#}

