import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Text.Read (readMaybe)

import qualified Y2023

firstYear :: Int
firstYear = 2023

years :: [[[IO ()]]]
years =
  [ Y2023.days
  ]

main :: IO ()
main = do
  args <- getArgs
  (y,d,n) <- case args of
    [y,d,n] -> maybe usage pure $ (,,) <$> readMaybe y <*> readMaybe d <*> readMaybe n
    _ -> usage
  let i'y = y - firstYear
  when (i'y < 0 || i'y >= length years) $
    die $ "unknown year: " <> show y
  let days = years !! i'y
      i'd = d - 1
  when (i'd < 0 || i'd >= length days) $
    die $ "unknown day: " <> show d
  let solutions = days !! i'd
  when (n < 0 || n >= length solutions) $
    die $ "unknown puzzle number (0 or 1): " <> show n
  solutions !! n

usage :: IO a
usage = do
  progName <- getProgName
  die $ "usage: " <> progName <> " YYYY DD N"
