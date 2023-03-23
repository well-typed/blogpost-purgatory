{-# OPTIONS_GHC -Wno-orphans #-}

module Certificate (
    Certificate -- opaque
  , SecretKey  -- opaque
  , selfSigned
  , certificateSubject
  , exampleKey
  , toPem
  , fromPem
  ) where

import Data.Text (Text)

import Data.Annotated
import Foreign.Rust.Marshall.Variable
import Foreign.Rust.Failure

import C.Certificate

-- | Generate new self-signed certificate
selfSigned :: [Text] -> IO (Either Failure (Certificate, SecretKey))
selfSigned = withBorshFailure . rustWrapperSelfSigned

-- | Certificate subject
certificateSubject :: Certificate -> Text
certificateSubject = withPureBorshVarBuffer . rustWrapperCertificateSubject

toPem :: SecretKey -> Text
toPem = withPureBorshVarBuffer . rustWrapperToPem

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
