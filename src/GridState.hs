module GridState where

import           Square
import           Data.List.Extra
import           Data.Tuple.Extra
import           Data.Maybe
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

import           Control.Monad.RWS

type Grid = Map Square Bool

newGrid :: Int -> Int -> Grid
newGrid w h | h < 0 || w < 0 = Map.empty
newGrid w h = Map.fromList . flip zip (repeat False) $ curry Square <$> [0..w] <*> [0..h]

resizeGrid :: Int -> Int -> Grid -> Grid
resizeGrid w h orig = foldl' (\g sq -> Map.insert sq (fromMaybe False $ orig !? sq) g) Map.empty $ curry Square <$> [0..w] <*> [0..h]

toggleSquares :: [Square] -> (Bool -> Bool) -> Grid -> Grid
toggleSquares list f orig = foldl' (\g sq -> Map.adjust f sq g) orig list

data Rule = Rule { born :: [Int], survive :: [Int] } deriving (Show)

conway :: Rule
conway = Rule [3] [2, 3]

applyRule :: Rule -> Square -> Grid -> Bool
applyRule Rule{..} sq@(Square (r, c)) grid
    | isNothing (grid !? sq) = False
    | otherwise =
        let surr = length . filter id . map (fromMaybe False . (grid !?)) $ surroundingSquares sq
            notInGrid = filter (isNothing . (grid !?)) $ surroundingSquares sq
        in  if   grid ! sq
            then surr `elem` survive
            else surr `elem` born

aliveList :: Grid -> [Square]
aliveList = filter <$> (!) <*> Map.keys

updateGrid :: Rule -> Grid -> Grid
updateGrid rule g = foldl' (\g' sq -> Map.insert sq (applyRule rule sq g) g') Map.empty $ Map.keys g

newtype Automaton a = Automaton { getAutomaton :: RWS Rule [String] Grid a }
    deriving (Functor, Applicative, Monad, MonadReader Rule, MonadWriter [String], MonadState Grid, MonadRWS Rule [String] Grid)

tick :: Automaton ()
tick = do
    r <- ask
    s <- get
    put $ updateGrid r s
    tell [show . length $ aliveList s]

runAutomaton :: Automaton a -> Rule -> Grid -> (a, Grid, [String])
runAutomaton = runRWS . getAutomaton

seedGlider :: Int -> Int -> Grid
seedGlider w h = foldl' (\g sq -> Map.insert (Square sq) True g) (newGrid w h) [(3, 1), (3, 2), (3, 3), (2, 3), (1, 2)]