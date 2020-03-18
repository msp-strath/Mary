module Shonkier.Semantics where

import Shonkier.Value

use :: Value -> Shonkier Computation
handle :: Request -> [Frame] -> Shonkier Computation
