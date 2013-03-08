{- |

Example matrix games.
   
From GHCi try:

>>> saddle m1
>>> saddle m2

-}

module Hagl.Examples.Matrix where

import Hagl

-- | A matrix game with two saddle points: (1,3) and (3,3).
m1 = matrix [1..4] [1..4] [  4,  3,  2,  5,
                           -10,  2,  0, -1,
                             7,  5,  2,  3,
                             0,  8, -4, -5 ]

-- | A matrix game with no saddle points.
m2 = matrix [1..3] [1..2] [  2, -3,
                             0,  2,
                            -5, 10 ]
