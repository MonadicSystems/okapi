{-# LANGUAGE StrictData #-}

module Chess where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data PieceType
  = Pawn
  | Knight
  | Rook
  | Bishop
  | Queen
  | King

data PieceColor = White | Black

type Piece = (PieceColor, PieceType)

wPawn = (White, Pawn)

wKnight = (White, Knight)

wRook = (White, Rook)

wBishop = (White, Bishop)

wQueen = (White, Queen)

wKing = (White, King)

bPawn = (Black, Pawn)

bKnight = (Black, Knight)

bRook = (Black, Rook)

bBishop = (Black, Bishop)

bQueen = (Black, Queen)

bKing = (Black, King)

data File
  = FileA
  | FileB
  | FileC
  | FileD
  | FileE
  | FileF
  | FileG
  | FileH
  deriving (Eq, Ord)

data Rank
  = Rank1
  | Rank2
  | Rank3
  | Rank4
  | Rank5
  | Rank6
  | Rank7
  | Rank8
  deriving (Eq, Ord)

type Position = (File, Rank)

a1 = (FileA, Rank1)

a2 = (FileA, Rank2)

a3 = (FileA, Rank3)

a4 = (FileA, Rank4)

a5 = (FileA, Rank5)

a6 = (FileA, Rank6)

a7 = (FileA, Rank7)

a8 = (FileA, Rank8)

b1 = (FileB, Rank1)

b2 = (FileB, Rank2)

b3 = (FileB, Rank3)

b4 = (FileB, Rank4)

b5 = (FileB, Rank5)

b6 = (FileB, Rank6)

b7 = (FileB, Rank7)

b8 = (FileB, Rank8)

c1 = (FileC, Rank1)

c2 = (FileC, Rank2)

c3 = (FileC, Rank3)

c4 = (FileC, Rank4)

c5 = (FileC, Rank5)

c6 = (FileC, Rank6)

c7 = (FileC, Rank7)

c8 = (FileC, Rank8)

d1 = (FileD, Rank1)

d2 = (FileD, Rank2)

d3 = (FileD, Rank3)

d4 = (FileD, Rank4)

d5 = (FileD, Rank5)

d6 = (FileD, Rank6)

d7 = (FileD, Rank7)

d8 = (FileD, Rank8)

e1 = (FileE, Rank1)

e2 = (FileE, Rank2)

e3 = (FileE, Rank3)

e4 = (FileE, Rank4)

e5 = (FileE, Rank5)

e6 = (FileE, Rank6)

e7 = (FileE, Rank7)

e8 = (FileE, Rank8)

f1 = (FileF, Rank1)

f2 = (FileF, Rank2)

f3 = (FileF, Rank3)

f4 = (FileF, Rank4)

f5 = (FileF, Rank5)

f6 = (FileF, Rank6)

f7 = (FileF, Rank7)

f8 = (FileF, Rank8)

g1 = (FileG, Rank1)

g2 = (FileG, Rank2)

g3 = (FileG, Rank3)

g4 = (FileG, Rank4)

g5 = (FileG, Rank5)

g6 = (FileG, Rank6)

g7 = (FileG, Rank7)

g8 = (FileG, Rank8)

h1 = (FileH, Rank1)

h2 = (FileH, Rank2)

h3 = (FileH, Rank3)

h4 = (FileH, Rank4)

h5 = (FileH, Rank5)

h6 = (FileH, Rank6)

h7 = (FileH, Rank7)

h8 = (FileH, Rank8)

data Board = Board {boardState :: Map Position Piece, boardHighlights :: [Position]}

startingBoard :: Board
startingBoard = Board (Map.fromList $ whiteFront <> whiteBack <> blackFront <> blackBack) []
  where
    whiteFront =
      [ (a2, wPawn),
        (b2, wPawn),
        (c2, wPawn),
        (d2, wPawn),
        (e2, wPawn),
        (f2, wPawn),
        (g2, wPawn),
        (h2, wPawn)
      ]
    blackFront =
      [ (a7, bPawn),
        (b7, bPawn),
        (c7, bPawn),
        (d7, bPawn),
        (e7, bPawn),
        (f7, bPawn),
        (g7, bPawn),
        (h7, bPawn)
      ]
    whiteBack =
      [ (a1, wRook),
        (b1, wKnight),
        (c1, wBishop),
        (d1, wQueen),
        (e1, wKing),
        (f1, wBishop),
        (g1, wKnight),
        (h1, wRook)
      ]
    blackBack =
      [ (a8, bRook),
        (b8, bKnight),
        (c8, bBishop),
        (d8, bQueen),
        (e8, bKing),
        (f8, bBishop),
        (g8, bKnight),
        (h8, bRook)
      ]
