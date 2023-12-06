module Y2023 (days) where

import qualified Y2023.D01
import qualified Y2023.D02
import qualified Y2023.D03
import qualified Y2023.D04
import qualified Y2023.D05
import qualified Y2023.D06

days :: [[IO ()]]
days =
  [ Y2023.D01.solutions
  , Y2023.D02.solutions
  , Y2023.D03.solutions
  , Y2023.D04.solutions
  , Y2023.D05.solutions
  , Y2023.D06.solutions
  ]
