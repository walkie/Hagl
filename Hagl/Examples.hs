-- | This module provides a convenient way to import all of the examples at once.
module Hagl.Examples (
  module Hagl.Examples.Auction,
  module Hagl.Examples.Crisis,
  module Hagl.Examples.Matrix,
  module Hagl.Examples.Prisoner,
  module Hagl.Examples.RPS
) where

import Hagl.Examples.Auction
import Hagl.Examples.Crisis
import Hagl.Examples.Matrix
import Hagl.Examples.Prisoner
import Hagl.Examples.RPS hiding (randy)
