module Y2023.D17 (solutions) where

import Prelude hiding (head)
import Data.Foldable (foldr')
import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (groupWith, head)
import Data.Maybe (mapMaybe)

import qualified Util.Map as Map

solutions :: [IO ()]
solutions = [ s1 steps [1..3], s1 ultraSteps [4..10] ]

s1 :: Stepper -> [Int] -> IO ()
s1 stepper endRuns =
  getContents
  >>= print . minimiseHeatLoss stepper endRuns . parseInput

minimiseHeatLoss :: Stepper -> [Int] -> Graph -> Int
minimiseHeatLoss stepper endRuns g =
  go Map.empty [StepState 0 start R 0, StepState 0 start D 0]
  where
  start = minimum $ Map.keys g
  dest  = maximum $ Map.keys g

  go seen locs =
    let
      stepOK (StepState acc loc dir n) =
        maybe True (acc <) (Map.lookup (loc,dir,n) seen)
      next =
        -- discard steps to seen blocks unless heat loss reduced
        filter stepOK

        -- extract least heat loss per (location,dir,count) tuple
        $ fmap head
        $ groupWith (\(StepState _ loc dir n) -> (loc,dir,n))
        $ sortBy (compare `on` (\(StepState x loc dir n) -> (loc,dir,n,x)))

        -- generate potential next steps
        $ locs >>= stepper g

      seen' = foldr' update seen next
    in
      case next of
        [] ->
          minimum
          $ mapMaybe (\k -> Map.lookup k seen')
          [ (dest,dir,n) | dir <- [U,D,L,R], n <- endRuns ]
        _  -> go seen' next

  update
    :: StepState
    -> Map.Map (Location, Direction, Int) HeatLoss
    -> Map.Map (Location, Direction, Int) HeatLoss
  update (StepState acc loc dir n) = Map.insertWith min (loc,dir,n) acc

back :: Direction -> Direction
back  = \case U -> D ; D -> U ; L -> R ; R -> L


-- | Function for getting allowed next steps
type Stepper = Graph -> StepState -> [StepState]

-- Step in all possible directions
steps, ultraSteps :: Stepper
steps = mkStepper applyDirNormal
ultraSteps = mkStepper applyDirUltra

type StepFilter =
     Direction  -- ^ Current direction
  -> Int        -- ^ Steps in current direction
  -> Direction  -- ^ Possible next direction
  -> (Direction -> Int -> StepState)
  -> Maybe StepState

mkStepper :: StepFilter -> Stepper
mkStepper applyDir g (StepState acc (y,x) dir n) =
  mapMaybe (applyHeatLoss g)
  $ mapMaybe (uncurry (applyDir dir n))
  $ [ (U, StepState acc (y - 1, x))
    , (D, StepState acc (y + 1, x))
    , (L, StepState acc (y, x - 1))
    , (R, StepState acc (y, x + 1))
    ]

applyDirNormal :: StepFilter
applyDirNormal dir n dir' f
  | dir' == dir, n >= 3 = Nothing
  | dir' == dir         = Just (f dir' (n + 1))
  | dir' == back dir    = Nothing
  | otherwise           = Just (f dir' 1)

applyDirUltra :: StepFilter
applyDirUltra dir n dir' f
  | dir' == dir, n >= 10  = Nothing
  | dir' == dir           = Just (f dir' (n + 1))
  | dir' == back dir      = Nothing
  | n < 4 {-change dir-}  = Nothing
  | otherwise             = Just (f dir' 1)

applyHeatLoss :: Graph -> StepState -> Maybe StepState
applyHeatLoss g (StepState acc loc dir n) =
  (\x -> StepState (acc + x) loc dir n) <$> Map.lookup loc g

type Graph = Map.Map Location Int

type Location = (Int,Int) -- (y,x)

data Direction = U | D | L | R
  deriving (Eq, Ord, Show)

type HeatLoss = Int

-- | How many steps taken in the current direction
data StepState = StepState HeatLoss Location Direction Int
  deriving (Show)

parseInput :: String -> Graph
parseInput s = Map.fromList $ cells
  where
  cells :: [(Location, Int)]
  cells = do
    (y,row) <- zip [0..] $ lines s
    (x,c) <- zip [0..] row
    pure ((y,x), read [c])
