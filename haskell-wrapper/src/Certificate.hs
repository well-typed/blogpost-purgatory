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

import C.Certificate

-- | Generate new self-signed certificate
selfSigned :: [Text] -> IO (Certificate, SecretKey)
selfSigned = withBorshVarBuffer . rustWrapperSelfSigned

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