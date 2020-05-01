module Shonkier.Semantics where

import Shonkier.Syntax
import Shonkier.Value

use :: Value -> Shonkier Computation
handle :: Request -> Continuation -> Shonkier Computation
complain :: Atom -> [Value] -> Shonkier Computation
