module C.Certificate (
    Certificate(..)
  , SecretKey(..)
  , genSelfSigned
  , certificateSubject
  ) where

#include "rust_wrapper.h"

import Codec.Borsh
import Data.ByteString qualified as Strict
import Data.FixedSizeArray (FixedSizeArray)
import Data.String
import Data.Text (Text)
import Data.Word

import Data.Structured qualified as Structured
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw
import Foreign.Rust.Serialisation.Raw.Base64

newtype Certificate = Certificate Strict.ByteString
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase64 Certificate

newtype SecretKey = SecretKey (FixedSizeArray 32 Word8)
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase64 SecretKey

{# fun unsafe rust_wrapper_generate_simple_self_signed as genSelfSigned
     { toBorshVar*  `[Text]'&
     , getVarBuffer `Buffer (Either Text (Certificate, SecretKey))'&
     }
  -> `()'
#}

{# fun unsafe rust_wrapper_get_certificate_subject as certificateSubject
     { toBorshVar*  `Certificate'&
     , getVarBuffer `Buffer Text'&
     }
  -> `()'
#}