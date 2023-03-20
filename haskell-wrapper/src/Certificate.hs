module Certificate (
    C.Certificate -- opaque
  , C.SecretKey  -- opaque
  , genSelfSigned
  , certificateSubject
  ) where

import Data.Text (Text)

import Foreign.Rust.Failure
import Foreign.Rust.Marshall.Variable

import C.Certificate qualified as C

-- | Generate new self-signed certificate
genSelfSigned :: [Text] -> IO (C.Certificate, C.SecretKey)
-- The use of 'throwFailureIO' here is justified because 'genSelfSigned' will
-- never generate an invalid certificate.
genSelfSigned = (>>= throwFailureIO) . withBorshFailure . C.genSelfSigned

-- | Certificate subject
certificateSubject :: C.Certificate -> Text
certificateSubject = withPureBorshVarBuffer . C.certificateSubject