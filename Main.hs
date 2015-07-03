module Main where

import Data.Char(chr)


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

data Tape a = Tape [a] a [a]

type Program = Tape Command


main = putStrLn "Hello!"

runProgram :: Program -> Tape Int -> IO ()
runProgram p t = putStrLn "Hello"

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

moveRight :: Tape a -> Tape a
moveRight (Tape ls c (r:rs)) = Tape (c:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) c rs) = Tape ls l (c:rs)
moveLeft t = error "Already at left end of tape"

incrementCell :: Tape Int -> Tape Int
incrementCell (Tape ls c rs) = Tape ls (c + 1) rs

decrementCell :: Tape Int -> Tape Int
decrementCell (Tape _ 0 _) = error "Cannot decrement cell with value 0"
decrementCell (Tape ls c rs) = Tape ls (c - 1) rs
