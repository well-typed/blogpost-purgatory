{-# OPTIONS_GHC -Wno-orphans #-}

module Certificate (
    Certificate -- opaque
  , SecretKey  -- opaque
  , genSelfSigned
  , certificateSubject
  , C.exampleKey
  , toPem
  , C.fromPem
  ) where

import Data.Text (Text)

import Foreign.Rust.Failure
import Foreign.Rust.Marshall.Variable

import C.Certificate (Certificate, SecretKey)
import C.Certificate qualified as C
import Data.Annotated

-- | Generate new self-signed certificate
genSelfSigned :: [Text] -> IO (Certificate, SecretKey)
-- The use of 'throwFailureIO' here is justified because 'genSelfSigned' will
-- never generate an invalid certificate.
genSelfSigned = (>>= throwFailureIO) . withBorshFailure . C.genSelfSigned

-- | Certificate subject
certificateSubject :: Certificate -> Text
certificateSubject = withPureBorshVarBuffer . C.certificateSubject

toPem :: SecretKey -> Text
toPem = withPureBorshVarBuffer . C.toPem

{-------------------------------------------------------------------------------
  Annotating 'Certificate'
-------------------------------------------------------------------------------}

deriving
  via PairWithAnnotation Certificate
  instance CanAnnotate Certificate

type instance Annotation Certificate = Text

instance ComputeAnnotation Certificate where
  computeAnnotation = certificateSubject


-- > data A = ..
-- >   deriving CanAnnotate via PairWithAnnotation A

--instance CanAnnotate Certificate where
