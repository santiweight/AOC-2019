{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Control.Monad.State

import Data.List.Split

import Data.Maybe

-- This should be a newtype
type OpCode = Int

data ProgramState = ProgramState { _codes :: [OpCode]
                                 , _offSet :: Int }
                    deriving Show

mkProgram = flip ProgramState 0

makeLenses ''ProgramState

advance :: MonadState ProgramState m => m ()
advance = offSet += 1

doOpCode :: MonadState ProgramState m => m ()
doOpCode = do
  item >>= \case
    1 -> do
      arg1 <- getArg
      arg2 <- getArg
      target <- item
      codes . ix target .= (arg1 + arg2)
      doOpCode
    2 -> do
      arg1 <- getArg
      arg2 <- getArg
      target <- item
      codes . ix target .= (arg1 * arg2)
      doOpCode
    99 -> return ()
    code -> do
      state <- get
      error . show =<< get

getArg :: MonadState ProgramState m => m OpCode
getArg = getIndex =<< item
  where
    getIndex index = fmap fromJust . preuse $ (codes . ix index)

item :: MonadState ProgramState m => m (Int)
item = do
  index <- use offSet
  num <- fmap fromJust . preuse $ (codes . ix index)
  advance
  return num

part1 = do
  contents <- readFile "/Users/santi/playground/AOC/day2/input.txt"
  let program = read <$> splitOn "," contents
                       & (ix 1 .~ 12)
                       & (ix 2 .~ 2)
  return . fromJust . preview (codes . ix 0) . execState doOpCode $ ProgramState program 0

part2 = do
  contents <- readFile "/Users/santi/playground/AOC/day2/input.txt"
  let program = fmap read . splitOn "," $ contents
  let go ((noun, verb):xs) =
        let codes' = program
                     & (ix 1 .~ noun)
                     & (ix 2 .~ verb)
            state = execState doOpCode $ mkProgram codes'
        in case state & preview (codes . ix 0) of
          Just 19690720 -> 100 * noun + verb
          _ -> go xs
      go _ = error ""
  let pairs = liftM2 (,) [0..99] [0..99]
  return $ go pairs

main = liftM2 (,) part1 part2
