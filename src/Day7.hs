{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Map as M
import qualified Data.Text as T
import Protolude
import Util (splitTwoOn)

type Name = Text

data Bag = Bag {bagName :: Name, bagCanHold :: CanHold} deriving (Show, Eq)

type CanHold = Map Name Int

type Db = Map Name Bag

parseCanHold :: Text -> Maybe (Name, Int)
parseCanHold canHoldT = case take 3 $ words canHoldT of
  [numT, word1, word2] -> do
    num <- (readMaybe . toS) numT
    let name = unwords [word1, word2]
    pure (name, num)
  _ -> Nothing

parseLine :: Text -> Maybe Bag
parseLine line = do
  (origBagT, canHoldT) <- splitTwoOn " bags contain " line
  let name = (unwords . take 2 . words) origBagT
  let canHoldL = T.splitOn ", " canHoldT
  let canHold = M.fromList $ mapMaybe parseCanHold canHoldL
  pure (Bag name canHold)

canItHold :: Name -> Db -> Name -> Bool
canItHold holdeeName db holderName = case M.lookup holderName db of
  Nothing -> False
  Just (Bag _ canHold) ->
    let subNames = M.keys canHold
     in holdeeName `elem` subNames || any (canItHold holdeeName db) subNames

mkDb :: Text -> Db
mkDb = M.fromList . fmap (\bag -> (bagName bag, bag)) . mapMaybe parseLine . lines

solveA :: Text -> Text
solveA input = (show . length . filter (canItHold "shiny gold" db)) allNames
  where
    db = mkDb input
    allNames = M.keys db

canHoldCapacity :: Db -> CanHold -> Int
canHoldCapacity db = M.foldrWithKey (\name cnt acc -> acc + cnt * (bagCapacity db name + 1)) 0

bagCapacity :: Db -> Name -> Int
bagCapacity db name = maybe 0 (canHoldCapacity db . bagCanHold) (M.lookup name db)

solveB :: Text -> Text
solveB input = show $ bagCapacity (mkDb input) "shiny gold"
