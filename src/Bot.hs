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
    scoutDir       :: [Dir]
  , scoutScore     :: Int
} deriving (Show, Eq)

data PosDir = PosDir {
    posDirPos :: Pos
  , posDirDir :: [Dir]
} deriving (Show, Eq)

scoutTheBoard :: State -> [Scout]
scoutTheBoard state = scoutSafeThePositions state posdir scout
  where
    board = gameBoard $ stateGame state
    scout = scoutsInit board 
    posdir = scoutNewDirs [(PosDir (heroPos $ stateHero state) [])] board scout 

scoutSafeThePositions :: State -> [PosDir] -> [Scout] -> [Scout]
scoutSafeThePositions  s pd scs 
  | length pd == 0 = scs
  | otherwise = scoutThePositions s pd scs 

scoutThePositions :: State -> [PosDir] -> [Scout] -> [Scout]
scoutThePositions  s pd scs
  | ta == Just FreeTile = scoutSafeThePositions s (scoutNewDirs pd b scs)  (addWay b scs hpd) 
  | ta == Just TavernTile = scoutSafeThePositions s pd' (addTavern b scs hpd (getMyLife s))
  | ta /= Just (HeroTile hid) && (ta == Just (HeroTile (HeroId 3)) || ta ==  Just (HeroTile (HeroId 2)) || ta == Just (HeroTile (HeroId 1)) || ta == Just (HeroTile (HeroId 0))) =  scoutSafeThePositions s pd' (addEnemy b scs hpd)
  | ta /= Just (MineTile $ Just hid) && (ta == Just (MineTile $ Just (HeroId 3)) || ta ==  Just (MineTile $ Just (HeroId 2)) || ta == Just (MineTile $ Just (HeroId 1)) || ta == Just (MineTile $ Just (HeroId 0)) || ta == Just (MineTile Nothing)) =  scoutSafeThePositions s pd' (addMine b scs hpd)
  | otherwise = scoutThePositions s pd' scs
  where
    b = gameBoard $ stateGame s
    hid = heroId $ stateHero s
    hpd = head pd
    pd' = tail pd
    p = posDirPos hpd
    ta = tileAt b p

getMyLife :: State -> Int
getMyLife s = fromIntegral $ heroLife $ stateHero s

scoutNewDirs :: [PosDir] -> Board -> [Scout] -> [PosDir]
scoutNewDirs opd b sc = pd ++ filter (scoutWasThere b sc) (map (\x -> PosDir (dirToPosition b p x) $ d ++ [x]) $ connections b p)
  where
    pd = tail opd
    (PosDir p d) = head opd

scoutWasThere :: Board -> [Scout] -> PosDir -> Bool
scoutWasThere b sc pd@(PosDir p d) 
  | inBoard b p == True = (scoutDir $ sc!!(posToIndex b p)) /= []
  | otherwise = True

addTavern :: Board -> [Scout] -> PosDir -> Int -> [Scout]
addTavern b s pd l = updateScout b s pd (100 - l)

addEnemy :: Board -> [Scout] -> PosDir -> [Scout]
addEnemy b s pd = updateScout b s pd (-40) 

addMine :: Board -> [Scout] -> PosDir -> [Scout]
addMine b s pd = updateScout b s pd (100)  

addWay :: Board -> [Scout] -> PosDir -> [Scout]
addWay b s pd = updateScout b s pd (0) 

updateScout :: Board -> [Scout] -> PosDir -> Int -> [Scout]
updateScout b s pd@(PosDir p d) i = a ++ f:[] ++ c
      where
        (a, e:c) = splitAt (posToIndex b p) s
        f = Scout d (i + scoutScore e)  

scoutTheBest :: [Scout] -> Dir
scoutTheBest sc = head $ scoutDir $ maximumBy orderScout sc

orderScout :: Scout -> Scout -> Ordering
orderScout s1 s2 = compare (scoutScore s1) (scoutScore s2)

scoutsInit :: Board -> [Scout]
scoutsInit b = map (\x -> (Scout [] 0)) $ [0 .. bs*bs]
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

