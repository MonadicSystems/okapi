{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lucid
import Data.Text
import Control.Monad

data PieceType
  = Pawn
  | Knight
  | Rook
  | Bishop
  | Queen
  | King
  deriving (Eq, Show)

data PieceColor = White | Black deriving (Eq, Ord, Show)

newtype Piece = Piece { unPiece :: (PieceColor, PieceType) } deriving (Eq, Show)

instance ToHtml Piece where
  toHtml (Piece (White, Pawn)) = "♙"
  toHtml (Piece (Black, Pawn)) = "♟"
  toHtml (Piece (White, Knight)) = "♘"
  toHtml (Piece (Black, Knight)) = "♞"
  toHtml (Piece (White, Rook)) = "♖"
  toHtml (Piece (Black, Rook)) = "♜"
  toHtml (Piece (White, Bishop)) = "♗"
  toHtml (Piece (Black, Bishop)) = "♝"
  toHtml (Piece (White, Queen)) = "♕"
  toHtml (Piece (Black, Queen)) = "♛"
  toHtml (Piece (White, King)) = "♔"
  toHtml (Piece (Black, King)) = "♚"
  toHtmlRaw = toHtml

wPawn = Piece (White, Pawn)

wKnight = Piece (White, Knight)

wRook = Piece (White, Rook)

wBishop = Piece (White, Bishop)

wQueen = Piece (White, Queen)

wKing = Piece (White, King)

bPawn = Piece (Black, Pawn)

bKnight = Piece (Black, Knight)

bRook = Piece (Black, Rook)

bBishop = Piece (Black, Bishop)

bQueen = Piece (Black, Queen)

bKing = Piece (Black, King)

data File
  = FileA
  | FileB
  | FileC
  | FileD
  | FileE
  | FileF
  | FileG
  | FileH
  deriving (Eq, Show, Ord)

data Rank
  = Rank1
  | Rank2
  | Rank3
  | Rank4
  | Rank5
  | Rank6
  | Rank7
  | Rank8
  deriving (Eq, Show, Ord)

newtype Position = Position { unPosition :: (File, Rank) } deriving (Eq, Ord)

a1 = Position (FileA, Rank1)
a2 = Position (FileA, Rank2)
a3 = Position (FileA, Rank3)
a4 = Position (FileA, Rank4)
a5 = Position (FileA, Rank5)
a6 = Position (FileA, Rank6)
a7 = Position (FileA, Rank7)
a8 = Position (FileA, Rank8)
b1 = Position (FileB, Rank1)
b2 = Position (FileB, Rank2)
b3 = Position (FileB, Rank3)
b4 = Position (FileB, Rank4)
b5 = Position (FileB, Rank5)
b6 = Position (FileB, Rank6)
b7 = Position (FileB, Rank7)
b8 = Position (FileB, Rank8)
c1 = Position (FileC, Rank1)
c2 = Position (FileC, Rank2)
c3 = Position (FileC, Rank3)
c4 = Position (FileC, Rank4)
c5 = Position (FileC, Rank5)
c6 = Position (FileC, Rank6)
c7 = Position (FileC, Rank7)
c8 = Position (FileC, Rank8)
d1 = Position (FileD, Rank1)
d2 = Position (FileD, Rank2)
d3 = Position (FileD, Rank3)
d4 = Position (FileD, Rank4)
d5 = Position (FileD, Rank5)
d6 = Position (FileD, Rank6)
d7 = Position (FileD, Rank7)
d8 = Position (FileD, Rank8)
e1 = Position (FileE, Rank1)
e2 = Position (FileE, Rank2)
e3 = Position (FileE, Rank3)
e4 = Position (FileE, Rank4)
e5 = Position (FileE, Rank5)
e6 = Position (FileE, Rank6)
e7 = Position (FileE, Rank7)
e8 = Position (FileE, Rank8)
f1 = Position (FileF, Rank1)
f2 = Position (FileF, Rank2)
f3 = Position (FileF, Rank3)
f4 = Position (FileF, Rank4)
f5 = Position (FileF, Rank5)
f6 = Position (FileF, Rank6)
f7 = Position (FileF, Rank7)
f8 = Position (FileF, Rank8)
g1 = Position (FileG, Rank1)
g2 = Position (FileG, Rank2)
g3 = Position (FileG, Rank3)
g4 = Position (FileG, Rank4)
g5 = Position (FileG, Rank5)
g6 = Position (FileG, Rank6)
g7 = Position (FileG, Rank7)
g8 = Position (FileG, Rank8)
h1 = Position (FileH, Rank1)
h2 = Position (FileH, Rank2)
h3 = Position (FileH, Rank3)
h4 = Position (FileH, Rank4)
h5 = Position (FileH, Rank5)
h6 = Position (FileH, Rank6)
h7 = Position (FileH, Rank7)
h8 = Position (FileH, Rank8)

data Board = Board {boardState :: Map Position Piece, boardHighlights :: [Position]}
  
instance ToHtml Board where
  toHtml (Board state highlights) = do
    div_ [class_ "grid grid-cols-8 grid-rows-8 gap-0 h-full w-full border-2 border-black"] $ do
      let blackSquareClass_ = class_ "flex items-center justify-center bg-gray-700 aspect-square text-3xl"
          whiteSquareClass_ = class_ "flex items-center justify-center bg-gray-200 aspect-square text-3xl"
      forM_
        ([(x, y) | x <- [1 .. 8], y <- [1 .. 8]])
        ( \(n, m) ->
            div_
              [ id_ $ tShow n <> tShow m
              , if odd n
                  then (if odd m then whiteSquareClass_ else blackSquareClass_)
                  else (if odd m then blackSquareClass_ else whiteSquareClass_)
              ]
              $ maybe "" toHtml (Map.lookup (xyToPosition (n, m)) state)
        )
  toHtmlRaw = toHtml

tShow :: Show a => a -> Text
tShow = pack . show

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

xyToPosition :: (Int, Int) -> Position
xyToPosition (x, y) = Position (intToFile y, intToRank (9 - x))
  where
    intToFile :: Int -> File
    intToFile 1 = FileA 
    intToFile 2 = FileB 
    intToFile 3 = FileC 
    intToFile 4 = FileD 
    intToFile 5 = FileE 
    intToFile 6 = FileF 
    intToFile 7 = FileG
    intToFile 8 = FileH
    intToFile _ = Prelude.error "No File for that Int exists"

    intToRank :: Int -> Rank
    intToRank 1 = Rank1
    intToRank 2 = Rank2 
    intToRank 3 = Rank3 
    intToRank 4 = Rank4 
    intToRank 5 = Rank5 
    intToRank 6 = Rank6 
    intToRank 7 = Rank7 
    intToRank 8 = Rank8
    intToRank _ = Prelude.error "No Rank for that Int exists"
