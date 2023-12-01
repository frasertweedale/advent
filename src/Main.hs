import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

import qualified Y2023

firstYear :: Int
firstYear = 2023

years :: [[IO ()]]
years =
  [ Y2023.days
  ]

main :: IO ()
main = do
  args <- getArgs
  (y,d) <- case args of
    [y,d] -> maybe usage pure $ (,) <$> readMaybe y <*> readMaybe d
    _ -> usage
  let i'y = y - firstYear
  when (i'y < 0 || i'y >= length years) $
    die $ "unknown year: " <> show y
  let days = years !! i'y
      i'd = d - 1
  when (i'd < 0 || i'd >= length days) $
    die $ "unknown day: " <> show d
  days !! i'd

usage :: IO a
usage = do
  progName <- getProgName
  die $ "usage: " <> progName <> " YYYY DD"
