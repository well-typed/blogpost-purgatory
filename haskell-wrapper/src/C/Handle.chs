module C.Handle (
    Handle
  , newHandle
  , handleId
  ) where

#include "rust_wrapper.h"

import Data.Word

{#pointer *Handle foreign finalizer rust_wrapper_free_handle newtype #}

{# fun unsafe rust_wrapper_new_handle as newHandle
      {
      }
   -> `Handle'
#}

{# fun pure unsafe rust_wrapper_handle_id as handleId
      { `Handle'
      }
   -> `Word64'
#}
