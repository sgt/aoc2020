{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day8 where

import qualified Data.IntSet as S
import qualified Data.Vector as V
import Protolude
import Util (parseSignedInt)

data Op = Nop | Jmp Int | Acc Int deriving (Show)

type Program = V.Vector Op

data VM = VM
  { vmAcc :: Int,
    vmIP :: Int,
    vmCycles :: Int,
    vmVisited :: IntSet,
    vmProgram :: Program
  }
  deriving (Show)

data VMStop = VMStop VMStopCause VM deriving (Show)

data VMStopCause = NormalTermination | IPOutOfRange | InfiniteLoop deriving (Show)

parseOp :: Text -> Maybe Op
parseOp line = case words line of
  ["nop", _] -> Just Nop
  ["acc", iT] -> Acc <$> parseSignedInt iT
  ["jmp", iT] -> Jmp <$> parseSignedInt iT
  _ -> Nothing

parseProgram :: Text -> Program
parseProgram = V.fromList . mapMaybe parseOp . lines

initVM :: Program -> VM
initVM = VM 0 0 0 S.empty

step :: VM -> Either VMStop VM
step vm@VM {..}
  | vmIP == length vmProgram = Left $ VMStop NormalTermination vm
  | vmIP `S.member` vmVisited = Left $ VMStop InfiniteLoop vm
  | otherwise = do
    op <- maybeToEither (VMStop IPOutOfRange vm) $ vmProgram V.!? vmIP
    let vm' = vm {vmVisited = S.insert vmIP vmVisited, vmCycles = vmCycles + 1}
    pure $ case op of
      Nop -> vm' {vmIP = vmIP + 1}
      Acc x -> vm' {vmAcc = vmAcc + x, vmIP = vmIP + 1}
      Jmp x -> vm' {vmIP = vmIP + x}

runUntilBreaks :: VM -> VMStop
runUntilBreaks = either identity runUntilBreaks . step

solveA :: Text -> Text
solveA input = show ex <> " " <> show vmAcc
  where
    (VMStop ex VM {vmAcc}) = (runUntilBreaks . initVM . parseProgram) input

solveB :: Text -> Text
solveB = undefined
