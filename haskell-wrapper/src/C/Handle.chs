module C.Handle (
    Handle
  , newHandle
  , handleId
  , freeHandle
  ) where

#include "rust_wrapper.h"

import Data.Word
import Foreign.Ptr

data Handle

{# fun unsafe rust_wrapper_new_handle as newHandle
      {
      }
   -> `Ptr Handle' castPtr
#}

{# fun pure unsafe rust_wrapper_handle_id as handleId
      { castPtr `Ptr Handle'
      }
   -> `Word64'
#}

{# fun unsafe rust_wrapper_free_handle as freeHandle
      { castPtr `Ptr Handle'
      }
   -> `()'
#}