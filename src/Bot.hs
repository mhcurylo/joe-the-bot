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
  return $ takeSafeDir $ scoutTheBest $ scoutTheBoard state

takeSafeDir :: [Dir] -> Dir
takeSafeDir xs
  | null xs == 0 = Stay
  | otherwise = head xs

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

data Value = Good | Neutral | Bad deriving (Show, Eq)

data Scout = Scout {
    scoutDir       :: [Dir]
  , scoutScore     :: Int
  , scoutValue     :: Value
  , scouted        :: Bool
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
  | null pd = resonate scs $ boardSize $ gameBoard $ stateGame s
  | otherwise = scoutThePositions s pd scs 

scoutThePositions :: State -> [PosDir] -> [Scout] -> [Scout]
scoutThePositions  s pd scs
  | i > 25 || (scoutWasThere b scs hpd) = scoutSafeThePositions s pd' scs
  | ta == Just FreeTile = scoutSafeThePositions s (scoutNewDirs pd b scs)  (addWay b scs hpd) 
  | ta == Just TavernTile = scoutSafeThePositions s pd' (addTavern b scs hpd (getMyLife s))
  | ta /= Just (HeroTile hid) && (ta == Just (HeroTile (HeroId 3)) || ta ==  Just (HeroTile (HeroId 2)) || ta == Just (HeroTile (HeroId 1)) || ta == Just (HeroTile (HeroId 0))) =  scoutSafeThePositions s pd' (addEnemy b scs hpd)
  | ta /= Just (MineTile $ Just hid) && (ta == Just (MineTile $ Just (HeroId 3)) || ta ==  Just (MineTile $ Just (HeroId 2)) || ta == Just (MineTile $ Just (HeroId 1)) || ta == Just (MineTile $ Just (HeroId 0)) || ta == Just (MineTile Nothing)) =  scoutSafeThePositions s pd' (addMine b scs hpd)
  | otherwise = scoutSafeThePositions s pd' scs
  where
    b = gameBoard $ stateGame s
    hid = heroId $ stateHero s
    hpd = head pd
    pd' = tail pd
    (PosDir p d) = hpd
    i = length d
    ta = tileAt b p

getMyLife :: State -> Int
getMyLife s = fromIntegral $ heroLife $ stateHero s

resonate :: [Scout] -> Int -> [Scout]
resonate scs b = fst $ foldl (\x y -> resonateValue x y) (scs, (0, b)) scs

resonateValue :: ([Scout], (Int, Int)) -> Scout -> ([Scout], (Int, Int))
resonateValue si@(scs, (i, b)) s
  | v == Neutral = (scs, (i + 1, b))
  | v == Bad = (resonateBadness si, (i + 1, b))
  | v == Good = (resonateGoodness si, (i + 1, b))
  where
    v = scoutValue s

resonateBadness :: ([Scout], (Int, Int)) -> [Scout]
resonateBadness si@(scs, (i, b)) = foldl (\x y -> moreScout (-10) y x) scs $ doubleKarma b i
resonateGoodness :: ([Scout], (Int, Int)) -> [Scout]
resonateGoodness si@(scs, (i, b)) = foldl (\x y -> moreScout 10 y x) scs $ doubleKarma b i 

moreScout :: Int -> Int -> [Scout] -> [Scout]
moreScout s i scs = a ++ f:c
      where
        (a, e:c) = splitAt i scs
        (Scout g h j y) = e
        f = Scout g (h + s) j y

doubleKarma :: Int -> Int -> [Int]
doubleKarma b i = concatMap wk $ wk i
  where
    wk = withinKarma b

withinKarma :: Int -> Int -> [Int]
withinKarma b i = filter (\x -> x /= -1) [idxn b i, idxs b i, idxw b i, idxe b i]

idxn :: Int -> Int -> Int
idxn b i 
  | b < i = i - b
  | otherwise = (-1)

idxs :: Int -> Int -> Int
idxs b i 
  | i <= b*(b - 1) = i + b
  | otherwise = (-1)

idxw :: Int -> Int -> Int
idxw b i 
  | i `mod` b /= 1 = i - 1
  | otherwise = (-1)

idxe :: Int -> Int -> Int
idxe b i 
  | i `mod` b /= 0 = i + 1
  | otherwise = (-1)

scoutNewDirs :: [PosDir] -> Board -> [Scout] -> [PosDir]
scoutNewDirs opd b sc = pd ++ filter (not . scoutWasThere b sc) (map (\x -> PosDir (dirToPosition b p x) $ d ++ [x]) $ connections b p)
  where
    pd = tail opd
    (PosDir p d) = head opd

scoutWasThere :: Board -> [Scout] -> PosDir -> Bool
scoutWasThere b sc pd@(PosDir p d) 
  | inBoard b p == True = (scouted $ sc!!(posToIndex b p)) == True
  | otherwise = True

addTavern :: Board -> [Scout] -> PosDir -> Int -> [Scout]
addTavern b s pd l
  | bonus == True  = update (250 - l*2) 
  | bonus == False = update (200 - l*2)
  where 
  bonus = length (posDirDir pd) < 3 && l < 90
  update = updateScout b s pd Good

addEnemy :: Board -> [Scout] -> PosDir -> [Scout]
addEnemy b s pd = updateScout b s pd Bad (-60) 

addMine :: Board -> [Scout] -> PosDir -> [Scout]
addMine b s pd = updateScout b s pd Good 100  

addWay :: Board -> [Scout] -> PosDir -> [Scout]
addWay b s pd = updateScout b s pd Neutral 0 

updateScout :: Board -> [Scout] -> PosDir -> Value -> Int -> [Scout]
updateScout b s pd@(PosDir p d) v i = a ++ f:[] ++ c
      where
        (a, e:c) = splitAt (posToIndex b p) s
        f = Scout d (i + (scoutScore e) - (length d) * 2) v True

scoutTheBest :: [Scout] -> [Dir]
scoutTheBest sc = scoutDir $ maximumBy orderScout sc

orderScout :: Scout -> Scout -> Ordering
orderScout s1 s2 = compare (scoutScore s1) (scoutScore s2)

scoutsInit :: Board -> [Scout]
scoutsInit b = map (\x -> (Scout [] 0 Neutral False)) $ [0 .. bs*bs]
  where 
    bs = boardSize b

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

