module C.Certificate (
    Certificate(..)
  , PrivateKey(..)
  , selfSigned
  ) where

#include "rust-wrapper.h"

import Codec.Borsh
import Data.Text (Text)
import Data.String
import Data.ByteString qualified as Strict

import Data.Structured qualified as Structured
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Serialisation.Raw
import Foreign.Rust.Serialisation.Raw.Base64

newtype Certificate = Certificate Strict.ByteString
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase64 Certificate

newtype PrivateKey = PrivateKey Strict.ByteString
  deriving newtype (BorshSize, ToBorsh, FromBorsh)
  deriving newtype (IsRaw)
  deriving (Show, Structured.Show, IsString) via AsBase64 PrivateKey

{# fun unsafe rust_wrapper_rcgen_generate_simple_self_signed as selfSigned
     { toBorshVar*  `[Text]'&
     , getVarBuffer `Buffer (Either Text (Certificate, PrivateKey))'&
     }
  -> `()'
#}