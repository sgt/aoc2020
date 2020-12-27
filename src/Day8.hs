{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day8 where

import qualified Data.IntSet as S
import qualified Data.Vector as V
import Protolude
import Util (parseSignedInt)

data Op = Nop | Jmp | Acc deriving (Show)

data Cmd = Cmd Op Int deriving (Show)

type Program = V.Vector Cmd

data VM = VM
  { vmAcc :: Int,
    vmIP :: Int,
    vmCycles :: Int,
    vmVisited :: IntSet,
    vmProgram :: Program
  }
  deriving (Show)

data VMStop = VMStop VMStopCause VM deriving (Show)

data VMStopCause = NormalTermination | IPOutOfRange | InfiniteLoop deriving (Show,Eq)

parseCmd :: Text -> Maybe Cmd
parseCmd line = case words line of
  [opT, numT] -> Cmd <$> str2op opT <*> parseSignedInt numT
  _ -> Nothing
  where
    str2op "nop" = Just Nop
    str2op "acc" = Just Acc
    str2op "jmp" = Just Jmp
    str2op _ = Nothing

parseProgram :: Text -> Program
parseProgram = V.fromList . mapMaybe parseCmd . lines

initVM :: Program -> VM
initVM = VM 0 0 0 S.empty

step :: VM -> Either VMStop VM
step vm@VM {..}
  | vmIP == length vmProgram = Left $ VMStop NormalTermination vm
  | vmIP `S.member` vmVisited = Left $ VMStop InfiniteLoop vm
  | otherwise = do
    cmd <- maybeToEither (VMStop IPOutOfRange vm) $ vmProgram V.!? vmIP
    let vm' = vm {vmVisited = S.insert vmIP vmVisited, vmCycles = vmCycles + 1}
    pure $ case cmd of
      Cmd Nop _ -> vm' {vmIP = vmIP + 1}
      Cmd Acc x -> vm' {vmAcc = vmAcc + x, vmIP = vmIP + 1}
      Cmd Jmp x -> vm' {vmIP = vmIP + x}

runUntilBreaks :: VM -> VMStop
runUntilBreaks = either identity runUntilBreaks . step

report::VMStop->Text
report (VMStop ex VM {vmAcc}) = show ex <> " " <> show vmAcc

solveA :: Text -> Text
solveA  = report. runUntilBreaks . initVM . parseProgram

fixProgram :: Program -> Int -> Maybe Program
fixProgram prog i = do
  (Cmd op n) <- prog V.!? i
  case op of
    Jmp -> pure $ upd Nop n
    Nop -> pure $ upd Jmp n
    _ -> Nothing
  where
    upd newOp n = prog V.// [(i, Cmd newOp n)]

solveB :: Text -> Text
solveB input = maybe "not found" report result
  where
    fixedPrograms prog = mapMaybe (fixProgram prog) [0 .. length prog -1]
    vmStops = map (runUntilBreaks . initVM) (fixedPrograms $ parseProgram input)
    result = find (\(VMStop ex _) -> ex == NormalTermination) vmStops
