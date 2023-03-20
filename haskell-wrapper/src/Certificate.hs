module Certificate (
    C.Certificate -- opaque
  , C.PrivateKey  -- opaque
  , selfSigned
  ) where

import Data.Text (Text)

import Foreign.Rust.Failure
import Foreign.Rust.Marshall.Variable

import C.Certificate qualified as C

selfSigned :: [Text] -> IO (Either Failure (C.Certificate, C.PrivateKey))
selfSigned = withBorshFailure . C.selfSigned
