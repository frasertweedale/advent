module Y2023 (days) where

import qualified Y2023.D01
import qualified Y2023.D02
import qualified Y2023.D03

days :: [[IO ()]]
days =
  [ Y2023.D01.solutions
  , Y2023.D02.solutions
  , Y2023.D03.solutions
  ]
