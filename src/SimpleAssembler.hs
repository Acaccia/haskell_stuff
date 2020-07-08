{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, BangPatterns  #-}
module SimpleAssembler (simpleAssembler) where
import           Control.Lens               hiding (index)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           Data.Vector                (Vector, fromList, (!?))

data Instruction = Mov String String | Inc String | Dec String | Jnz String String
type Registers = M.Map String Int
type Program = Vector Instruction

data ProgramState = ProgramState { _registers :: !Registers, _index :: !Int }
makeLenses ''ProgramState

instance Read Instruction where
  readsPrec _ instr = case words instr of
    ["mov", x, y] -> [(Mov x y, "")]
    ["inc", x]    -> [(Inc x, "")]
    ["dec", x]    -> [(Dec x, "")]
    ["jnz", x, y] -> [(Jnz x y, "")]
    _             -> []

simpleAssembler :: [String] -> Registers
simpleAssembler = runReader (evalStateT runProgram (ProgramState M.empty 0)) . fromList . map read

runInstruction :: MonadState ProgramState m => Instruction -> m ()
runInstruction (Mov x y) = case readNumber y of
          Just n  -> registers %= M.insert x n
          Nothing -> registers %= (M.insert x =<< (M.! y))
runInstruction (Inc x) = registers %= M.adjust (+ 1) x
runInstruction (Dec x) = registers %= M.adjust (subtract 1) x
runInstruction (Jnz x y) = do b <- case readNumber x of
                                     Just n  -> pure n
                                     Nothing -> uses registers (M.! x)
                              when (b /= 0) $ do
                                step <- case readNumber y of
                                          Just m -> pure m
                                          Nothing -> uses registers (M.! y)
                                index += step - 1

incrementIndex :: MonadState ProgramState m => m ()
incrementIndex = index += 1

runProgram :: (MonadState ProgramState m, MonadReader Program m) => m Registers
runProgram = do i <- use index
                p <- ask
                case p !? i of
                  Nothing -> use registers
                  Just instr -> runInstruction instr *> incrementIndex *> runProgram

readNumber :: String -> Maybe Int
readNumber xs = case reads xs of
  [(x, "")] -> Just x
  _         -> Nothing
