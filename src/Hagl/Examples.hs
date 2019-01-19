-- | This module provides a convenient way to import all of the examples at once.
module Hagl.Examples (
  module Hagl,
  module Hagl.Examples.Auction,
  module Hagl.Examples.Beauty,
  module Hagl.Examples.Chance,
  module Hagl.Examples.Crisis,
  module Hagl.Examples.Matches,
  module Hagl.Examples.Matrix,
  module Hagl.Examples.Prisoner,
  module Hagl.Examples.RPS,
  module Hagl.Examples.TicTacToe
) where

import Hagl
import Hagl.Examples.Auction
import Hagl.Examples.Beauty
import Hagl.Examples.Chance
import Hagl.Examples.Crisis
import Hagl.Examples.Matches hiding (randy)
import Hagl.Examples.Matrix
import Hagl.Examples.Prisoner
import Hagl.Examples.RPS hiding (randy)
import Hagl.Examples.TicTacToe
