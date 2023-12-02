module Y2023 (days) where

import qualified Y2023.D01
import qualified Y2023.D02

days :: [[IO ()]]
days =
  [ Y2023.D01.solutions
  , Y2023.D02.solutions
  ]
