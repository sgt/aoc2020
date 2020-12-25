module Day5 where

import qualified Data.Text as T
import Protolude

data Seat = Seat Int Int deriving (Show)

decodeBinary :: Char -> Text -> Int
decodeBinary ch1 = T.foldl (\acc x -> acc * 2 + fromEnum (x == ch1)) 0

decodeSeat :: Text -> Seat
decodeSeat x = Seat (decodeBinary 'B' rowCode) (decodeBinary 'R' seatCode)
  where
    (rowCode, seatCode) = T.splitAt 7 x

seatId :: Seat -> Int
seatId (Seat row col) = row * 8 + col

allSeatIds :: Text -> [Int]
allSeatIds = fmap (seatId . decodeSeat) . lines

solveA :: Text -> Text
solveA = show . maximum . allSeatIds

solveB :: Text -> Text
solveB input = show maybeResult
  where
    ids = sort $ allSeatIds input
    zipped = zip ids (drop 1 ids)
    maybeResult = do
      (prev, _) <- find (\(x, y) -> y /= x + 1) zipped
      pure (prev + 1)
