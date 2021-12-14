module Main where
import Data.List
import Text.Read
data Cell =
  X | O | Empty -- data constructors
  deriving (Eq)
instance Show Cell where -- using Haskell Show typeclass to convert cell values into strings
  show X = "X"
  show O = "O"
  show Empty = " "

type Board = [Cell] -- creating list of cells forming a player board, row major index

printBoard :: Board -> String -- type signature
-- pattern matching since we know board is exactly 9 cells
-- creating a string that can be printed on the terminal
-- pattern case:
printBoard [cell0, cell1, cell2,
            cell3, cell4, cell5,
            cell6, cell7, cell8] =
          show cell0 ++ "|" ++ show cell1 ++ "|" ++ show cell2 ++ "\n" ++
          "-----" ++ "\n" ++
          show cell3 ++ "|" ++ show cell4 ++ "|" ++ show cell5 ++ "\n" ++
          "-----" ++ "\n" ++
          show cell6 ++ "|" ++ show cell7 ++ "|" ++ show cell8 ++ "\n"

-- function to place new player move on the board, takes in board, index, and X/O
playerMove :: Board -> Int -> Cell -> Maybe Board
playerMove board place player
  = let (first, second) = splitAt place board -- splitting the list of cells at the given index
        secondTail = tail second -- removing the index's value and returning the rest of the list
        newSecondTail = player : secondTail -- appending the new value to the beginning of the second list
        updatedList = first ++ newSecondTail -- combining both lists together
        isEmpty = head second == Empty -- bool value to check if index is currently empty

    in if isEmpty then Just updatedList else Nothing --if isEmpty is true, then return updatedList, if not nothing       using the Maybe type constructor

isThereWinnerRow :: Board -> Bool
isThereWinnerRow board =
                ((board!!0 == board!!1) && (board!!0 == board!!2)) && (board!!0 /= Empty)||
                ((board!!3 == board!!4) && (board!!3 == board!!5)) && (board!!3 /= Empty)||
                ((board!!6 == board!!7) && (board!!6 == board!!8)) && (board!!6 /= Empty)

isThereWinnerCol:: Board -> Bool
isThereWinnerCol board =
                ((board!!0 == board!!3) && (board!!0 == board!!6)) && (board!!0 /= Empty)||
                ((board!!1 == board!!4) && (board!!1 == board!!7)) && (board!!1 /= Empty)||
                ((board!!2 == board!!5) && (board!!2 == board!!8)) && (board!!2 /= Empty)

isThereWinnerDiag:: Board -> Bool
isThereWinnerDiag board =
                ((board!!0 == board!!4) && (board!!0 == board!!8)) && (board!!0 /= Empty)||
                ((board!!2 == board!!4) && (board!!2 == board!!6)) && (board!!2 /= Empty)

isThereWinner:: Board -> Bool
isThereWinner board = isThereWinnerRow board || isThereWinnerCol board || isThereWinnerDiag board

nextPlayer:: Cell -> Cell
nextPlayer X = O -- pattern matching
nextPlayer O = X

playRound :: Board -> Cell -> IO() -- similar to void
playRound board player = do
  putStrLn $ (show player) ++ "'s Turn"
  putStr "Please pick a cell 0-8: "
  line <- getLine
  case readMaybe line of
    Nothing -> putStrLn "Invalid choice, please try again." >> playRound board player
    Just num -> if num >= 0 && num <= 8
                then case playerMove board num player of
                  Nothing -> putStrLn "Invalid choice, please choose a cell that is empty." >> playRound board player
                  Just list -> do
                    putStrLn(printBoard list)
                    if isThereWinner list then putStrLn $ "Player " ++ (show player) ++ " has won!"
                                          else playRound list (nextPlayer player)
                else putStrLn "Invalid choice, please enter a value from 0 - 8." >> playRound board player

main :: IO()
main = do
  putStrLn "Welcome to Tic Tac Toe!"
  putStrLn "The board is indicated by the following numbers:"
  putStrLn "0|1|2"
  putStrLn "-----"
  putStrLn "3|4|5"
  putStrLn "-----"
  putStrLn "6|7|8"

  let board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

  putStrLn (printBoard board)
  playRound board X
