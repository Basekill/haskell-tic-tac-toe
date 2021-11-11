module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read
import Debug.Trace

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show, Enum)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

constructBoard :: Int -> Board
constructBoard n
  = ([Empty | count <- [1..n*n]], n)
  
-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver board
  = or rs || or cs || or ds 
  where
    rs = checkLine rows 
    cs = checkLine cols 
    ds = checkLine diags 
    checkLine :: (Board -> [[Cell]]) -> [Bool]
    checkLine lines 
      = [nub line == [Taken O] || nub line == [Taken X]  | line <- (lines board)]

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition pos
  = readMaybe pos' :: Maybe Position
  where
    pos' = '(' : takeWhile (not . isSpace) pos ++ "," ++ dropWhile (not . isSpace) pos ++ ")"

parseSize :: String -> Maybe Int
parseSize size
  | n > 0 = Just n 
  | otherwise = Nothing
  where
    n = fromMaybe 0 (readMaybe size :: Maybe Int)

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove plr (i, j) (cells, n) 
  | i < 0 || j < 0 || i >= n || j >= n || (cells !! index) /= Empty = Nothing
  | otherwise = Just (replace index (Taken plr) cells, n) 
  where
    index = i * n + j

-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint board
  = putStr (concat lines)
  where
    lines = [intersperse ' ' (prettyPrint' row) | row <- rows board]
    prettyPrint' :: [Cell] -> String
    prettyPrint' []
      = "\n"
    prettyPrint' (x : xs)
      | x == Empty = '-' : prettyPrint' xs
      | x == Taken O = 'O' : prettyPrint' xs
      | otherwise = 'X' : prettyPrint' xs

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

doParseAction :: (String -> Maybe a) -> String -> IO a
doParseAction parse errorMsg
  = do
       line <- getLine
       let input = parse line
       -- need to also check the validity of the given input
       maybe (do 
                putStr errorMsg
                doParseAction parse errorMsg) return input

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board plr
  = do
                      putStr ("Player " ++ show plr ++ ", make your move (row col): ")
                      pos <- doParseAction parsePosition "Invalid move, try again: "
                      let board' = tryMove plr pos board
                      maybe (do 
                                putStrLn "Invalid position"
                                takeTurn board plr) return board'
    
-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board plr
  = do
      prettyPrint board 
      board' <- takeTurn board plr
      if gameOver board'
        then do
               prettyPrint board'
               putStrLn ("Player " ++ show plr ++ " has won!")
               putStrLn ("Thank you for playing")
        else playGame board' (toEnum (((fromEnum plr) + 1) `mod` 2))


-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do 
      putStrLn "Welcome to tic tac toe on an N x N board"
      putStr "Enter the board size (N): "
      n <- doParseAction parseSize "Invalid board size, try again: "
      let board = constructBoard n 
      playGame (constructBoard n) X


-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
