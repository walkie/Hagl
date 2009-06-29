-- From GHCi try:
-- > saddle m1
-- > saddle m2
-- To get the "saddle points" of a matrix game.  Saddle points correspond to
-- pure Nash equilibria.  (m2 doesn't have any saddle points, m1 has two.)

module Examples.Matrix where

import Hagl.Normal

m1 = matrix [1..4] [1..4] [  4,  3,  2,  5,
                           -10,  2,  0, -1,
                             7,  5,  2,  3,
                             0,  8, -4, -5 ]

m2 = matrix [1..3] [1..2] [  2, -3,
                             0,  2,
                            -5, 10 ]
