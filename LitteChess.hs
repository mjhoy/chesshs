{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LitteChess where

import Data.Array.IArray
import Data.List
import Data.List.Split

data Player = Black | White
            deriving (Show, Eq)

data Piece = Pawn

data Square = Square Player Piece
            | Empty

newtype Rank = Rank { _getRank :: Int }
             deriving (Read, Num, Enum, Integral, Real, Eq, Ord, Ix)

instance Show Rank where
  show (Rank a) = show a

newtype File = File { _getFile :: Int }
             deriving (Read, Num, Enum, Eq, Ord, Ix)

instance Show File where
  show (File a) = [['a'..'z'] !! a]

-- Define our board's boundaries
instance Bounded File where
  maxBound = 1
  minBound = 0

instance Bounded Rank where
  maxBound = 1
  minBound = 0  

domain :: (Bounded a, Enum a) => [a]
domain = [minBound..maxBound]

instance Show Square where
  show (Square White Pawn) = "♙"
  show (Square Black Pawn) = "♟"
  show Empty = "·"

type Pos = (Rank,File)  

type BoardArray = Array Pos Square

initialBoard :: BoardArray
initialBoard = array ((minBound,minBound),(maxBound,maxBound)) initialBoardArray
  where
    coordinatePairs :: [(Rank, File)]
    coordinatePairs = [ (x,y) | x <- domain, y <- domain ]
    initialBoardArray = zip coordinatePairs initialBoardArray'
    initialBoardArray' = concat $ map reverse $ [
      [ (Square Black Pawn), Empty ],
      [ Empty, (Square White Pawn) ]
      ]

-- λ putStrLn $ showBoard initialBoard
-- ·♙
-- ♟·
showBoard :: BoardArray -> String
showBoard board = boardWithNewlines
  where
    n = (maxBound :: Rank)
    boardWithNewlines = addNewline boardWithRanks
    boardWithRanks = chunk (fromIntegral n) (show $ elems board)
    addNewline = intercalate "\n"

data Game = Game {
  _board :: BoardArray,
  _turn :: Player } deriving (Show)

initialGame :: Game
initialGame = Game initialBoard White

data Gamestate = Legal Game
               | Illegal
               deriving (Show)

checkOccupied :: BoardArray -> Pos -> Player -> Bool
checkOccupied board pos player = case board ! pos of
                                  Empty -> False
                                  (Square player' _) -> player == player'

move :: Game -> Pos -> Gamestate
move game dest = case (checkOccupied board dest player) of
                  True -> Illegal
                  False -> Legal game
  where
    board = _board game
    player = _turn game
