module Chess where

import Control.Monad.ST
import Control.Monad (foldM_)
import Data.Array.ST

data Player = Black | White
            deriving (Show)

data Piece = Pawn
           | Knight
           | Bishop
           | Rook
           | Queen
           | King
             deriving (Show)

data Square = Square Player Piece
            | Empty

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
          deriving (Show, Eq, Ord, Enum, Bounded, Ix)

data File = FA | FB | FC | FD | FE | FF | FG | FH
          deriving (Show, Eq, Ord, Enum, Bounded, Ix)

instance Show Square where
  show (Square White King)   = "♔"
  show (Square White Queen)  = "♕"
  show (Square White Rook)   = "♖"
  show (Square White Bishop) = "♗"
  show (Square White Knight) = "♘"
  show (Square White Pawn)   = "♙"
  show (Square Black King)   = "♚"
  show (Square Black Queen)  = "♛"
  show (Square Black Rook)   = "♜"
  show (Square Black Bishop) = "♝"
  show (Square Black Knight) = "♞"
  show (Square Black Pawn)   = "♟"
  show Empty = "·"

type BoardArray s = STArray s (Rank, File) Square

initialABoard :: ST s (BoardArray s)
initialABoard = do
    arr <- newArray ((R1, FA), (R8, FH)) Empty
    foldM_ updateBoard (arr, R1, FA) $ concat initialBoardArray
    return arr
  where
    updateBoard (arr, rank, FH) square = do
      writeArray arr (rank, FH) square
      return (arr, succ rank, FA)
    updateBoard (arr, rank, file) square = do
      writeArray arr (rank, file) square
      return (arr, rank, succ file)

    -- ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
    -- ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
    -- · · · · · · · ·
    -- · · · · · · · ·
    -- · · · · · · · ·
    -- · · · · · · · ·
    -- ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
    -- ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
    initialBoardArray = [
      blackPieces, blackPawns,
      empty, empty, empty, empty,
      whitePawns, whitePieces
      ]
    pawns = take 8 $ repeat Pawn
    empty = take 8 $ repeat Empty
    toBlack = Square Black
    toWhite = Square White
    pieces = [ Rook
             , Knight
             , Bishop
             , Queen
             , King
             , Bishop
             , Knight
             , Rook ]
    blackPieces = map toBlack pieces
    blackPawns  = map toBlack pawns
    whitePieces = map toWhite pieces
    whitePawns  = map toWhite pawns

-- ghci:
-- let arr = runSTArray initialABoard
-- arr ! (R8, FE)
-- > ♔
