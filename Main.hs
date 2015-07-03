module Main where

import Data.Char(chr, ord)
import System.Environment(getArgs)


data Command = IncrementP
             | DecrementP
             | IncrementC
             | DecrementC
             | Print
             | Read
             | LoopL
             | LoopR
             | Other
             | End  -- Used to represent end of program
             deriving (Show, Eq)

data Tape a = Tape [a] a [a] deriving Show

type Program = Tape Command

blankTape :: Tape Int
blankTape = Tape [] 0 zeros
                where zeros = repeat 0


-- Primary "let's do things" functions
--
main = do
    args <- getArgs
    prog <- readFile $ head args
    runProgram (parse prog) blankTape


runProgram :: Program -> Tape Int -> IO ()

runProgram (Tape _ End _) _ = return ()
runProgram p@(Tape _ IncrementC _) t = runProgram (moveRight p) (incrementCell t)
runProgram p@(Tape _ DecrementC _) t = runProgram (moveRight p) (decrementCell t)
runProgram p@(Tape _ IncrementP _) t = runProgram (moveRight p) (moveRight t)
runProgram p@(Tape _ DecrementP _) t = runProgram (moveRight p) (moveLeft t)

runProgram p@(Tape _ Print _) t = do
    printCell t
    runProgram (moveRight p) t

runProgram p@(Tape _ Read _) t = do
    newT <- readCell t
    runProgram (moveRight p) newT

runProgram p@(Tape _ LoopL _) t@(Tape _ x _)
    | x == 0    = runProgram (seekLoopR p) t
    | otherwise = runProgram (moveRight p) t

runProgram p@(Tape _ LoopR _) t@(Tape _ x _)
    | x /= 0    = runProgram (seekLoopL p) t
    | otherwise = runProgram (moveRight p) t


-- General tape manipulation functions
--
moveRight :: Tape a -> Tape a
moveRight (Tape ls c (r:rs)) = Tape (c:ls) r rs
moveRight t = error "Already at right end of tape"

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) c rs) = Tape ls l (c:rs)
moveLeft t = error "Already at left end of tape"


-- Program-tape-only functions
--
seekLoopL :: Program -> Program
seekLoopL p = seekLoopL' (moveLeft p) 0

seekLoopL' :: Program -> Int -> Program
seekLoopL' t@(Tape _ LoopL _) x
    | x == 0    = moveRight t
    | otherwise = seekLoopL' (moveLeft t) (x - 1)
seekLoopL' t@(Tape _ LoopR _) x = seekLoopL' (moveLeft t) (x + 1)
seekLoopL' t x = seekLoopL' (moveLeft t) x

seekLoopR :: Program -> Program
seekLoopR p = seekLoopR' (moveRight p) 0

seekLoopR' :: Program -> Int -> Program
seekLoopR' t@(Tape _ LoopR _) x
    | x == 0    = moveRight t
    | otherwise = seekLoopR' (moveRight t) (x - 1)
seekLoopR' t@(Tape _ LoopL _) x = seekLoopR' (moveRight t) (x + 1)
seekLoopR' t x = seekLoopR' (moveRight t) x


-- Storage-tape-only functions
--
incrementCell :: Tape Int -> Tape Int
incrementCell (Tape ls c rs) = Tape ls (c + 1) rs

decrementCell :: Tape Int -> Tape Int
decrementCell (Tape _ 0 _) = error "Cannot decrement cell with value 0"
decrementCell (Tape ls c rs) = Tape ls (c - 1) rs

printCell :: Tape Int -> IO ()
printCell (Tape _ c _) = print $ chr c

readCell :: Tape Int -> IO (Tape Int)
readCell (Tape ls _ rs) = do
    c <- getChar
    return (Tape ls (ord c) rs)


-- Parsing stuff
--
charToCommand :: Char -> Command
charToCommand '>' = IncrementP
charToCommand '<' = DecrementP
charToCommand '+' = IncrementC
charToCommand '-' = DecrementC
charToCommand '.' = Print
charToCommand ',' = Read
charToCommand '[' = LoopL
charToCommand ']' = LoopR
charToCommand x = Other

parse :: String -> Program
parse s = Tape [] c (cs ++ [End])
    where (c:cs) = filter (/= Other) $ map charToCommand s

