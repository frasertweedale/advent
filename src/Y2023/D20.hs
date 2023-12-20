module Y2023.D20 (solutions) where

import Control.Applicative
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe, isJust)
import Data.List (find, findIndex, partition)
import Data.List.NonEmpty (groupAllWith)
import qualified Data.List.NonEmpty as NonEmpty

import Data.Foldable (toList)

import qualified Util.Map as Map
import Util.Parser

solutions :: [IO ()]
solutions = [s1 go1, s1 go2]

s1 :: (Show a) => (Input -> a) -> IO ()
s1 go =
  getContents
  >>= print . go . initialise
      . maybe (error "parse failed") fst . runParser parseInput

go1 :: Input -> Int
go1 =
  (\(h,l) -> length h * length l)
  . partition (\(pulse,_,_) -> pulse == High)
  . concat
  . fmap snd
  . take 1000
  . drop 1
  . iterate push . (,[])
  where
    push (state, _) = pushButton state

type PulsePattern = (Pulse, String, String) -- ^ (pulse, src, dst)

go2 :: Input -> Int
go2 nw =
  -- The least-common-multiple of the number of cycles to each
  -- required pulse occurrence is the result.
  foldr1 lcm $ fmap pushesToPulse requiredPulses
  where
    -- Determine the number of button pushes for a given pulse to occur.
    pushesToPulse :: PulsePattern -> Int
    pushesToPulse pat =
      fromMaybe (-1)
      . findIndex (isJust . find (== pat))
      . fmap snd
      . iterate (\(state, _) -> pushButton state)
      $ (nw,[])

    sourcesFor :: String -> [(String, (String, (Module, [String])))]
    sourcesFor dst =
      fmap (dst,)
      <$> filter (\(_,(_,dsts)) -> dst `elem` dsts) (Map.toList nw)

    -- Inspect the acyclic part of the graph leading to "rx".
    -- Extract the set of pulse conditions that result in low
    -- pulse to "rx" through this acyclic segment of the graph.
    requiredPulses :: [PulsePattern]
    requiredPulses = sourcesFor "rx" >>= go Low
      where
      go pulse (dst, (src, v)) = case v of
        (Conjunction memory, [_]) ->
          -- Conjunction, only one output
          case (pulse, Map.keys memory) of
            (Low, srcSrcs) ->
              -- All inbound pulses must be High
              srcSrcs >>= \srcSrc -> case Map.lookup srcSrc nw of
                Nothing -> error "can't happen"
                Just v' -> go High (src, (srcSrc, v'))
            (High, [srcSrc]) ->
              -- Single-input Conjunction, i.e. "inverter".
              -- Input signal must be "Low"
              case Map.lookup srcSrc nw of
                Nothing -> error "can't happen"
                Just v' -> go Low (src, (srcSrc, v'))
            _ ->
              -- Too much uncertainty; terminate here
              pure (pulse, src, dst)
        _ ->
          -- Source module is not a Conjunction, or has multiple
          -- destinations.  Too much uncertainty; terminate here.
          pure (pulse, src, dst)

type Input = Network
type Network = Map.Map String (Module, [String])

data Module
  = FlipFlop OnOff
  | Conjunction (Map.Map String Pulse)
  | Broadcast
  | Sink
  deriving (Show)

data OnOff = On | Off
  deriving (Eq, Ord, Show)

data Pulse = Low | High
  deriving (Eq, Ord, Show)

-- | Initialise Conjunction states.
--
initialise :: Input -> Input
initialise network =
  foldr updateSources network
  . groupAllWith fst                          -- group (dst,src) pairs by dst
  . (>>= \(src,(_,dsts)) -> fmap (,src) dsts) -- generate (dst,src) pairs
  . Map.toList
  $ network
  where
    updateSources
      :: NonEmpty.NonEmpty (String, String)
      -> Map.Map String (Module, b)
      -> Map.Map String (Module, b)
    updateSources pairs nw =
      let
        dst = fst $ NonEmpty.head pairs
        memory =
          foldr (\(_,src) -> Map.insert src Low) Map.empty (toList pairs)
      in
        case Map.lookup dst nw of
          Just ((Conjunction _, dsts))
            -> Map.insert dst ((Conjunction memory, dsts)) nw
          _ -> nw

-- | Press the button, returning a list of the pulses delivered
-- and the final network state.
--
pushButton :: Network -> ( Network, [(Pulse, String, String)] )
pushButton = go (enqueue (Low, "button", broadcaster) newQueue)
  where
  go q nw = case dequeue q of
    Nothing -> ( nw, [] )
    Just ((pulse, src, dst), q') -> case Map.lookup dst nw of
      Nothing ->
        -- receiving module not found, record pulse and continue
        ((pulse,src,dst):) <$> go q' nw
      Just (dstMod, dstDsts) ->
        let
          (dstMod', mPulse') = deliverPulse src pulse dstMod
          nw' = Map.insert dst (dstMod', dstDsts) nw
          q'' = case mPulse' of
            Nothing -> q'
            Just pulse' ->
              foldl (flip enqueue) q'
              $ fmap (\dstDst -> (pulse', dst, dstDst)) dstDsts
        in
          ((pulse,src,dst):) <$> go q'' nw'

deliverPulse
  :: String                 -- ^ source module
  -> Pulse
  -> Module                 -- ^ receiving module
  -> (Module, Maybe Pulse)  -- ^ updated module state and pulse, if any
deliverPulse src pulse m = case (m, pulse) of
  (Broadcast, _)      -> (Broadcast, Just pulse)
  (FlipFlop Off, Low) -> (FlipFlop On, Just High)
  (FlipFlop On,  Low) -> (FlipFlop Off, Just Low)
  (Conjunction states, _) ->
    let
      states' = Map.insert src pulse states
      allHigh = all ((== High) . snd) (Map.toList states')
    in
      (Conjunction states', Just (if allHigh then Low else High))
  _                   -> (m, Nothing)

broadcaster :: String
broadcaster = "broadcaster"

parseInput :: Parser Input
parseInput = do
  Map.fromList . toList
    <$> parseModule `sepBy` char '\n'
  where
  parseModule = do
    (\(modType, k) dests -> (k, (modType, dests)))
      <$> modAndName <* string " -> "
      <*> (toList <$> (name `sepBy` string ", "))

  name = some (satisfy isAlpha)

  modAndName =
    (Broadcast, broadcaster)      <$ string broadcaster
    <|> (FlipFlop Off,)           <$ char '%' <*> name
    <|> (Conjunction Map.empty,)  <$ char '&' <*> name
    <|> (Sink,)                   <$ char '.' <*> name


data Queue a = Queue [a] [a]
  deriving (Show)

newQueue :: Queue a
newQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue a (Queue h t) = Queue h (a:t)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue = \case
  Queue (a:h) t -> Just (a, Queue h t)
  Queue _ []    -> Nothing
  Queue _ t     -> dequeue (Queue (reverse t) [])
