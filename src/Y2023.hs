module Y2023 (days) where

import qualified Y2023.D14
import qualified Y2023.D13
import qualified Y2023.D12
import qualified Y2023.D11
import qualified Y2023.D10
import qualified Y2023.D09
import qualified Y2023.D08
import qualified Y2023.D07
import qualified Y2023.D06
import qualified Y2023.D05
import qualified Y2023.D04
import qualified Y2023.D03
import qualified Y2023.D02
import qualified Y2023.D01

days :: [[IO ()]]
days = reverse $ tail
  [ []  -- simplify editing;
  , Y2023.D14.solutions
  , Y2023.D13.solutions
  , Y2023.D12.solutions
  , Y2023.D11.solutions
  , Y2023.D10.solutions
  , Y2023.D09.solutions
  , Y2023.D08.solutions
  , Y2023.D07.solutions
  , Y2023.D06.solutions
  , Y2023.D05.solutions
  , Y2023.D04.solutions
  , Y2023.D03.solutions
  , Y2023.D02.solutions
  , Y2023.D01.solutions
  ]
