-- This module provides a convenient way to import most of Hagl all at once.
-- Modules relating to particular types of games should be imported separately,
-- since they contain overlapping function names.  Game-specific modules include
-- Hagl.Extensive, Hagl.Normal, and Hagl.StateBased.
module Hagl (
  module Hagl.Accessor,
  module Hagl.Core,
  module Hagl.Exec,
  module Hagl.Game,
  module Hagl.GameTree,
  module Hagl.Print,
  module Hagl.Selector,
  module Hagl.Strategy
) where

import Hagl.Accessor
import Hagl.Core
import Hagl.Exec
import Hagl.Game
import Hagl.GameTree
import Hagl.Print
import Hagl.Selector
import Hagl.Strategy
