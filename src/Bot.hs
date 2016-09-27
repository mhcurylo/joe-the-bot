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
  return $ scoutTheBest $ scoutTheBoard state

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
  , scoutScore     :: Int
  , scoutIteration :: Int
} deriving (Show, Eq)

scoutTheBoard :: State -> [Scout]
scoutTheBoard state = scoutSafeThePositions state posdir scout 0 
  where
    board = gameBoard $ stateGame state
    scout = scoutsInit board 
    posdir = scoutNewDirs [] board scout (heroPos $ stateHero state)

scoutSafeThePositions :: State -> [(Pos, Dir)] -> [Scout] -> Int -> [Scout]
scoutSafeThePositions  s pd scs i
  | length pd == 0 = scs
  | i == 1000 = scs
  | otherwise = scoutThePositions s pd scs i

scoutThePositions :: State -> [(Pos, Dir)] -> [Scout] -> Int -> [Scout]
scoutThePositions  s pd scs i
  | length pd == 0 = scs
  | ta == Just FreeTile = scoutSafeThePositions s (scoutNewDirs pd' b scs ap)  (addWay b scs ap d i) i' 
  | ta == Just TavernTile = scoutSafeThePositions s pd' (addTavern b scs ap d (getMyLife s) i) i'
  | ta /= Just (HeroTile hid) && (ta == Just (HeroTile (HeroId 3)) || ta ==  Just (HeroTile (HeroId 2)) || ta == Just (HeroTile (HeroId 1)) || ta == Just (HeroTile (HeroId 0))) =  scoutSafeThePositions s pd' (addEnemy b scs ap d i) i'
  | ta /= Just (MineTile $ Just hid) && (ta == Just (MineTile $ Just (HeroId 3)) || ta ==  Just (MineTile $ Just (HeroId 2)) || ta == Just (MineTile $ Just (HeroId 1)) || ta == Just (MineTile $ Just (HeroId 0)) || ta == Just (MineTile Nothing)) =  scoutSafeThePositions s pd' (addMine b scs ap d i) i'
  | otherwise = scoutThePositions s pd' scs i'
  where
    b = gameBoard $ stateGame s
    hid = heroId $stateHero s
    hpd = head pd
    p = fst hpd
    d = snd hpd
    ap = dirToPosition b p d
    ta = tileAt b ap
    isAt = isTileAtPos b ap
    pd' = tail pd
    i' = i + 1


getMyLife :: State -> Int
getMyLife s = fromIntegral $ heroLife $ stateHero s

scoutNewDirs :: [(Pos, Dir)] -> Board -> [Scout] -> Pos -> [(Pos, Dir)]
scoutNewDirs pd b sc p = pd ++ (filter (scoutWasThere b sc) $  map (\x -> (p, x)) $ connections b p)

scoutWasThere :: Board -> [Scout] -> (Pos, Dir) -> Bool
scoutWasThere b sc pd@(p, d) 
  | inBoard b pos == True = (scoutDir $ sc!!(posToIndex b pos)) == Stay
  | otherwise = True  
  where
  pos = dirToPosition b p d

addTavern :: Board -> [Scout] -> Pos -> Dir -> Int -> Int -> [Scout]
addTavern b s p d l i = updateScout b s p d (100 - l)  i

addEnemy :: Board -> [Scout] -> Pos -> Dir -> Int -> [Scout]
addEnemy b s p d i = updateScout b s p d (-40) i

addMine :: Board -> [Scout] -> Pos -> Dir -> Int -> [Scout]
addMine b s p d i = updateScout b s p d (100) i 

addWay :: Board -> [Scout] -> Pos -> Dir -> Int -> [Scout]
addWay b s p d i = updateScout b s p d (0) i 

updateScout :: Board -> [Scout] -> Pos -> Dir -> Int -> Int -> [Scout]
updateScout b s p d sp i = a ++ f:[] ++ c
      where
        (a, e:c) = splitAt (posToIndex b p) s
        f = Scout d (scoutScore e + sp) i  

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

