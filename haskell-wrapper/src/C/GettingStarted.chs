module C.GettingStarted (rustWrapperAdd) where

#include "rust_wrapper.h"

import Data.Word

{# fun pure unsafe rust_wrapper_add as rustWrapperAdd
     { `Word64'
     , `Word64'
     }
  -> `Word64'
#}
