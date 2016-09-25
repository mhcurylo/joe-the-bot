module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

bot :: Bot
bot = myBot

myBot :: Bot
myBot state = do
  liftIO $ putStrLn $ show $ nearestGold state
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
    idx = y * boardSize b + x

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx

nearestGold :: State -> [Dir]
nearestGold state = connections (gameBoard $ stateGame state) (heroPos $ stateHero state)

connections :: Board -> Pos -> [Dir]  
connections b p = filter (canTraverseTo b p) [North, South, West, East]

canTraverseTo :: Board -> Pos -> Dir -> Bool
canTraverseTo b p d  
  | isJust t = not (fromJust t == WoodTile)
  | isNothing t = False
  where
    t = tileAt b $ dirToPosition b p d

dirToPosition :: Board -> Pos -> Dir -> Pos
dirToPosition  b p@(Pos x y) d 
  | d == West = Pos (x-1) y
  | d == East = Pos (x+1) y
  | d == North = Pos x (y-1)
  | d == South = Pos x (y+1)


