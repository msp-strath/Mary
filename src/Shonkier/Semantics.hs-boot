module Shonkier.Semantics where

import Shonkier.Value

use :: Value -> Shonkier Computation
handle :: Request -> Continuation -> Shonkier Computation
complain :: String -> [Value] -> Shonkier Computation
