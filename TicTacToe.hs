module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read
import Debug.Trace

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq)

instance Show Cell where
  show Empty = "-"
  show (Taken X) = "X"
  show (Taken O) = "O"

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


  
-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver board
  = or (checkLine rows) || or (checkLine cols) || or (checkLine diags)
  where
    checkLine :: (Board -> [[Cell]]) -> [Bool]
    checkLine lines 
      = [True | [Taken _] <- map nub (lines board)]

isFull :: Board -> Bool
isFull (cells, _)
  = Empty `notElem` cells

switchPlayer :: Player -> Player
switchPlayer O
  = X
switchPlayer X
  = O

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition pos
  = readMaybe ('(' : a ++ "," ++ b ++ ")") :: Maybe Position
  where
    (a, b) = break isSpace pos

-- Given an input string determines whether it is a positive integer
parseSize :: String -> Maybe Int
parseSize size
  = filterMaybe (>0) (readMaybe size :: Maybe Int)

-- Tries to write the given player's marker at the given board position
tryMove :: Player -> Position -> Board -> Maybe Board
tryMove plr (i, j) (cells, n) 
  | i < 0 || j < 0 || i >= n || j >= n || (cells !! index) /= Empty = Nothing
  | otherwise = Just (replace index (Taken plr) cells, n) 
  where
    index = i * n + j

-------------------------------------------------------------------
-- I/O Functions

-- Prints a given board
prettyPrint :: Board -> IO ()
prettyPrint board
  = putStr (concat lines)
  where
    lines = [intersperse ' ' (prettyPrint' row) | row <- rows board]
    prettyPrint' :: [Cell] -> String
    prettyPrint' []
      = "\n"
    prettyPrint' (x : xs)
      = show x ++ prettyPrint' xs

doParseAction :: String -> (String -> Maybe a) -> IO a
doParseAction errorMsg parse
  = do
       line <- getLine
       let input = parse line
       maybe (do 
                putStr errorMsg
                doParseAction errorMsg parse) return input

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board plr
  = do
      putStr ("Player " ++ show plr ++ ", make your move (row col): ")
      doParseAction "Invalid move, try again: " (\line -> parsePosition line >>= (\pos -> tryMove plr pos board))
    
-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board plr
  = do
      prettyPrint board 
      board' <- takeTurn board plr
      case (gameOver board', isFull board') of
        (True, _) -> displayEnd ("Player " ++ show plr ++ " has won!") board'
        (_, True) -> displayEnd "It's a draw!" board'
        _         -> playGame board' (switchPlayer plr)
  where
    displayEnd :: String -> Board -> IO ()
    displayEnd msg board
      = do
          prettyPrint board
          putStrLn msg
          putStrLn ("Thank you for playing")
 
-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do 
      putStrLn "Welcome to tic tac toe on an N x N board"
      putStr "Enter the board size (N): "
      n <- doParseAction "Invalid board size, try again: " parseSize
      playGame (constructBoard n) X
  where
    -- constructs an empty board of size n
    constructBoard :: Int -> Board
    constructBoard n
      = ([Empty | count <- [1..n*n]], n)



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

testBoard4
  = ([Taken X,Taken O,Taken X,
      Taken X,Taken O,Taken O,
      Taken O,Taken X,Taken X],
      3 :: Int)
