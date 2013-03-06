-- | This module provides a convenient way to import all of the examples at once.
module Hagl.Examples (module X) where

import Hagl                   as X
import Hagl.Examples.Auction  as X
import Hagl.Examples.Chance   as X
import Hagl.Examples.Crisis   as X
import Hagl.Examples.Matrix   as X
import Hagl.Examples.Prisoner as X
import Hagl.Examples.RPS      as X hiding (randy)

