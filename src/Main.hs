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
  years !! (y - firstYear) !! (d - 1)

usage :: IO a
usage = do
  progName <- getProgName
  die $ "usage: " <> progName <> " YYYY DD"
