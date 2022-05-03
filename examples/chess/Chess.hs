{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lucid
import Data.Text
import Control.Monad
import Web.HttpApiData
import Lucid.Htmx

data PieceType
  = Pawn
  | Knight
  | Rook
  | Bishop
  | Queen
  | King
  deriving (Eq, Show)

data PieceColor = White | Black deriving (Eq, Ord, Show)

opposite :: PieceColor -> PieceColor
opposite White = Black
opposite Black = White

newtype Piece = Piece { unPiece :: (PieceColor, PieceType) } deriving (Eq)

instance Show Piece where
  show (Piece piece) = case piece of
    (White, Pawn) -> "WP"
    (Black, Pawn) -> "BP"
    (White, Knight) -> "WN"
    (Black, Knight) -> "BN"
    (White, Rook) -> "WR"
    (Black, Rook) -> "BR"
    (White, Bishop) -> "WB"
    (Black, Bishop) -> "BB"
    (White, Queen) -> "WQ"
    (Black, Queen) -> "BQ"
    (White, King) -> "WK"
    (Black, King) -> "BK"

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

instance ToHttpApiData Piece where
  toUrlPiece = pack . show
  toQueryParam  = toUrlPiece

instance FromHttpApiData Piece where
  parseUrlPiece piece = case piece of
    "WP" -> Right wPawn
    "BP" -> Right bPawn
    "WN" -> Right wKnight
    "BN" -> Right bKnight
    "WR" -> Right wRook
    "BR" -> Right bRook
    "WB" -> Right wBishop
    "BB" -> Right bBishop
    "WQ" -> Right wQueen
    "BQ" -> Right bQueen
    "WK" -> Right wKing
    "BK" -> Right bKing
    _ -> Left "Couldn't parse Piece"
  parseQueryParam  = parseUrlPiece

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
  deriving (Eq, Show, Ord, Enum)

data Rank
  = Rank1
  | Rank2
  | Rank3
  | Rank4
  | Rank5
  | Rank6
  | Rank7
  | Rank8
  deriving (Eq, Show, Ord, Enum)

newtype Position = Position { unPosition :: (File, Rank) } deriving (Eq, Ord)

instance Show Position where
  show (Position position) = case position of
    (FileA, Rank1) -> "a1"
    (FileA, Rank2) -> "a2"
    (FileA, Rank3) -> "a3"
    (FileA, Rank4) -> "a4"
    (FileA, Rank5) -> "a5"
    (FileA, Rank6) -> "a6"
    (FileA, Rank7) -> "a7"
    (FileA, Rank8) -> "a8"
    (FileB, Rank1) -> "b1"
    (FileB, Rank2) -> "b2"
    (FileB, Rank3) -> "b3"
    (FileB, Rank4) -> "b4"
    (FileB, Rank5) -> "b5"
    (FileB, Rank6) -> "b6"
    (FileB, Rank7) -> "b7"
    (FileB, Rank8) -> "b8"
    (FileC, Rank1) -> "c1"
    (FileC, Rank2) -> "c2"
    (FileC, Rank3) -> "c3"
    (FileC, Rank4) -> "c4"
    (FileC, Rank5) -> "c5"
    (FileC, Rank6) -> "c6"
    (FileC, Rank7) -> "c7"
    (FileC, Rank8) -> "c8"
    (FileD, Rank1) -> "d1"
    (FileD, Rank2) -> "d2"
    (FileD, Rank3) -> "d3"
    (FileD, Rank4) -> "d4"
    (FileD, Rank5) -> "d5"
    (FileD, Rank6) -> "d6"
    (FileD, Rank7) -> "d7"
    (FileD, Rank8) -> "d8"
    (FileE, Rank1) -> "e1"
    (FileE, Rank2) -> "e2"
    (FileE, Rank3) -> "e3"
    (FileE, Rank4) -> "e4"
    (FileE, Rank5) -> "e5"
    (FileE, Rank6) -> "e6"
    (FileE, Rank7) -> "e7"
    (FileE, Rank8) -> "e8"
    (FileF, Rank1) -> "f1"
    (FileF, Rank2) -> "f2"
    (FileF, Rank3) -> "f3"
    (FileF, Rank4) -> "f4"
    (FileF, Rank5) -> "f5"
    (FileF, Rank6) -> "f6"
    (FileF, Rank7) -> "f7"
    (FileF, Rank8) -> "f8"
    (FileG, Rank1) -> "g1"
    (FileG, Rank2) -> "g2"
    (FileG, Rank3) -> "g3"
    (FileG, Rank4) -> "g4"
    (FileG, Rank5) -> "g5"
    (FileG, Rank6) -> "g6"
    (FileG, Rank7) -> "g7"
    (FileG, Rank8) -> "g8"
    (FileH, Rank1) -> "h1"
    (FileH, Rank2) -> "h2"
    (FileH, Rank3) -> "h3"
    (FileH, Rank4) -> "h4"
    (FileH, Rank5) -> "h5"
    (FileH, Rank6) -> "h6"
    (FileH, Rank7) -> "h7"
    (FileH, Rank8) -> "h8"

instance ToHttpApiData Position where
  toUrlPiece = pack . show
  toQueryParam = toUrlPiece

instance FromHttpApiData Position where
  parseUrlPiece position = case position of
    "a1" -> Right a1
    "a2" -> Right a2
    "a3" -> Right a3
    "a4" -> Right a4
    "a5" -> Right a5
    "a6" -> Right a6
    "a7" -> Right a7
    "a8" -> Right a8
    "b1" -> Right b1
    "b2" -> Right b2
    "b3" -> Right b3
    "b4" -> Right b4
    "b5" -> Right b5
    "b6" -> Right b6
    "b7" -> Right b7
    "b8" -> Right b8
    "c1" -> Right c1
    "c2" -> Right c2
    "c3" -> Right c3
    "c4" -> Right c4
    "c5" -> Right c5
    "c6" -> Right c6
    "c7" -> Right c7
    "c8" -> Right c8
    "d1" -> Right d1
    "d2" -> Right d2
    "d3" -> Right d3
    "d4" -> Right d4
    "d5" -> Right d5
    "d6" -> Right d6
    "d7" -> Right d7
    "d8" -> Right d8
    "e1" -> Right e1
    "e2" -> Right e2
    "e3" -> Right e3
    "e4" -> Right e4
    "e5" -> Right e5
    "e6" -> Right e6
    "e7" -> Right e7
    "e8" -> Right e8
    "f1" -> Right f1
    "f2" -> Right f2
    "f3" -> Right f3
    "f4" -> Right f4
    "f5" -> Right f5
    "f6" -> Right f6
    "f7" -> Right f7
    "f8" -> Right f8
    "g1" -> Right g1
    "g2" -> Right g2
    "g3" -> Right g3
    "g4" -> Right g4
    "g5" -> Right g5
    "g6" -> Right g6
    "g7" -> Right g7
    "g8" -> Right g8
    "h1" -> Right h1
    "h2" -> Right h2
    "h3" -> Right h3
    "h4" -> Right h4
    "h5" -> Right h5
    "h6" -> Right h6
    "h7" -> Right h7
    "h8" -> Right h8
    _    -> Left "Couldn't parse Position"
  parseQueryParam = parseUrlPiece

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

newtype Board = Board { unBoard :: Map Position Piece }

instance ToHtml Board where
  toHtml (Board board) = do
    div_ [class_ "grid grid-cols-8 grid-rows-8 gap-0 h-full w-full border-2 border-black"] $ do
      forM_
        ([encodingToPosition (encodedFile, encodedRank) | encodedFile <- [0 .. 7], encodedRank <- [0 .. 7]])
        ( \position -> do
            let tileClass = positionToTileClass position
            case Map.lookup position board of
              Just piece -> do
                div_
                  [ id_ $ tShow position
                  , tileClass
                  , hxGet_ $ "/select?position=" <> toQueryParam position <> "&piece=" <> toUrlPiece piece
                  , hxSwap_ "outerHTML"
                  ]
                  $ toHtml piece
              Nothing -> div_ [tileClass] ""
        )
  toHtmlRaw = toHtml

tShow :: Show a => a -> Text
tShow = pack . show

startingBoard :: Board
startingBoard = Board (Map.fromList $ whiteFront <> whiteBack <> blackFront <> blackBack)
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

encodingToPosition :: (Int, Int) -> Position
encodingToPosition (x, y) = Position (toEnum y, toEnum (7 - x))

calculatePossibleMoves :: Board -> Position -> Piece -> [Position]
calculatePossibleMoves (Board board) (Position (file, rank)) (Piece (pieceColor, pieceType)) = undefined

performMove :: Board -> Position -> Piece -> (Board, Maybe Piece)
performMove (Board board) position piece = undefined

positionToTileClass :: Position -> Attribute
positionToTileClass (Position (file, rank)) =
  class_ $ "flex items-center justify-center aspect-square text-3xl " <>
    case (even $ fromEnum file, even $ fromEnum rank) of
      (True, True) -> "bg-gray-700"
      (True, False) -> "bg-gray-200"
      (False, False) -> "bg-gray-700"
      (False, True) -> "bg-gray-200"
