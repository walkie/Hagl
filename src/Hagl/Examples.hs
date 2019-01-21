-- | This module provides a convenient way to import all of the examples at once.
module Hagl.Examples (
  module Hagl,
  module Hagl.Example.Auction,
  module Hagl.Example.Beauty,
  module Hagl.Example.Chance,
  module Hagl.Example.Crisis,
  module Hagl.Example.Matches,
  module Hagl.Example.Matrix,
  module Hagl.Example.Prisoner,
  module Hagl.Example.RPS,
  module Hagl.Example.TicTacToe
) where

import Hagl
import Hagl.Example.Auction
import Hagl.Example.Beauty
import Hagl.Example.Chance
import Hagl.Example.Crisis
import Hagl.Example.Matches hiding (randy)
import Hagl.Example.Matrix
import Hagl.Example.Prisoner
import Hagl.Example.RPS hiding (randy)
import Hagl.Example.TicTacToe
