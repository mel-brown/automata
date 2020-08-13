module Square where

import Data.Tuple.Extra

newtype Square = Square { unSquare :: (Int, Int) } deriving (Eq, Ord, Read, Show)

row, col :: Square -> Int
row (Square (r, _)) = r
col (Square (_, c)) = c

manhattan :: Square -> Square -> Int
manhattan (Square (x1, y1)) (Square (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

squareUp, squareDown, squareLeft, squareRight :: Square -> Square
squareUp    = Square . first  (subtract 1) . unSquare
squareDown  = Square . first  (+ 1)        . unSquare
squareLeft  = Square . second (subtract 1) . unSquare
squareRight = Square . second (+ 1)        . unSquare

squareUL, squareDL, squareUR, squareDR :: Square -> Square
squareUL = Square . both (subtract 1)     . unSquare
squareDL = Square . ((+1) *** subtract 1) . unSquare
squareUR = Square . (subtract 1 *** (+1)) . unSquare
squareDR = Square . both (+ 1)            . unSquare

-- | Get the eight squares surrounding this one.
surroundingSquares :: Square -> [Square]
surroundingSquares = ([squareUL, squareUp, squareUR, squareLeft, squareRight, squareDL, squareDown, squareDR] <*>) . pure

-- | Get the four squares adjacent to this one in each cardinal direction.
adjacentSquares :: Square -> [Square]
adjacentSquares = ([squareUp, squareLeft, squareRight, squareDown] <*>) . pure

-- | Tests if the three points lie on the same line.
collinear :: Square -> Square -> Square -> Bool
collinear (Square (r1,c1)) (Square (r2,c2)) (Square (r3,c3))
    | r1 == r2 = r1 == r3
    | otherwise =
        let m = fromIntegral (c2 - c1) / fromIntegral (r2 - r1)
            b = fromIntegral c1 - (m * fromIntegral r1)
        in  c3 == round (m * fromIntegral r3 + b)

-- | Tests if the third square lies on the line segment joining the first two.
between :: Square -> Square -> Square -> Bool
between sq1@(Square (r1, c1)) sq2@(Square (r2, c2)) sq3@(Square (r3, c3)) = collinear sq1 sq2 sq3 &&
    compare r1 r3 == compare r3 r2 &&
    compare c1 c3 == compare c3 c2
