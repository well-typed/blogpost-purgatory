module Handle (
    Handle -- opaque
  , newHandle
  ) where

import C.Handle qualified as C
import System.IO.Unsafe
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)

newtype Handle = Handle (ForeignPtr C.Handle)

instance Show Handle where
  show (Handle fptr) = unsafePerformIO $
      withForeignPtr fptr $ \ptr ->
        return $ "<Handle " ++ show (C.handleId ptr) ++ ">"

newHandle :: IO Handle
newHandle = do
    ptr <- C.newHandle
    Handle <$> newForeignPtr ptr (C.freeHandle ptr)
