{-# OPTIONS_GHC -Wno-orphans #-}

module Handle (
    Handle -- opaque
  , newHandle
  ) where

import C.Handle

instance Show Handle where
  show h = "<Handle " ++ show (handleId h) ++ ">"
