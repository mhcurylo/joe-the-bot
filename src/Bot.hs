module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (liftM)
import Data.List (maximumBy)
import Control.Monad.IO.Class (liftIO)

bot :: Bot
bot = myBot

myBot :: Bot
myBot state = do
  liftIO $ putStrLn $ show $ nearestGold state
  liftIO $ putStrLn $ show $ scoutTheBest $ scoutTheBoard state 
  return North

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = posToIndex b p

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx

data Scout = Scout {
    scoutDir       :: Dir
  , scoutScore     :: Integer
  , scoutIteration :: Integer
} deriving (Show, Eq)



scoutTheBoard :: State -> [Scout]
scoutTheBoard state = scoutsInit $ gameBoard $ stateGame state

scoutThePositions :: Board -> [Pos] -> [Scout] -> [Scout]
  b p scs 
  | isAt FreeTile = scoutThePositions b p scs 

  where
   isAt = isTileAtPos b $ first p


scoutTheBest :: [Scout] -> Dir
scoutTheBest sc = scoutDir $ maximumBy orderScout sc

orderScout :: Scout -> Scout -> Ordering
orderScout s1 s2 = compare (scoutScore s1) (scoutScore s2)

scoutsInit :: Board -> [Scout]
scoutsInit b = map (\x -> (Scout Stay 0 0)) $ [0 .. bs*bs]
   where 
    bs = boardSize b

nearestGold :: State -> [Dir]
nearestGold state = connections (gameBoard $ stateGame state) (heroPos $ stateHero state)

connections :: Board -> Pos -> [Dir]  
connections b p = filter (canTraverseTo b p) [North, South, West, East]

isTileAtPos :: Board -> Pos -> Tile -> Bool
isTileAtPos b p t 
  | isJust ta = fromJust ta == t 
  | isNothing ta = False
  where
    ta = tileAt b p

isOfTile :: Board -> Pos -> Dir -> Tile -> Bool
isOfTile b p d t = isTileAtPos b dp t
  where
    dp = dirToPosition b p d

canTraverseTo :: Board -> Pos -> Dir -> Bool
canTraverseTo b p d = not $ isOfTile b p d WoodTile  

dirToPosition :: Board -> Pos -> Dir -> Pos
dirToPosition  b p@(Pos x y) d 
  | d == West = Pos (x-1) y
  | d == East = Pos (x+1) y
  | d == North = Pos x (y-1)
  | d == South = Pos x (y+1)

posToIndex :: Board -> Pos -> Int
posToIndex b p@(Pos x y) = y * boardSize b + x 

