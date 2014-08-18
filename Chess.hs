module Chess where

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
  
                       
newtype Board = Board [[Square]]

instance Show Board where
  show (Board rows) = concat $ map ((++ "\n") . showRow) rows
    where
      showRow squares = concat $ map ((++ " ") . show) squares


# ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
# ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
# · · · · · · · ·
# · · · · · · · ·
# · · · · · · · ·
# · · · · · · · ·
# ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
# ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
newGameBoard :: Board
newGameBoard = Board [
    blackPieces, blackPawns,
    empty, empty, empty, empty,
    whitePawns, whitePieces
    ]
  where
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
