module Main (main) where

import Sudoku.Pretty
import System.Environment
import Data.List
import System.TimeIt

main :: IO ()
main = commandLineInterface

commandLineInterface :: IO ()
commandLineInterface = timeIt $ do
    (fileIn : fileOut : _) <- (++ repeat "") <$> getArgs
    board <- read <$>
        if null fileIn
            then getContents
            else readFile fileIn
    let output = message (solve board)
    if null fileOut
        then putStrLn output
        else writeFile fileOut output

showAllSolutions :: Bool
showAllSolutions = False

message :: [PrettySudoku] -> String
message []     = "Impossible"
message (x:xs) = show x ++
    if not showAllSolutions || null xs
        then ""
        else "\n\nOther Solutions: " ++
            show (length xs) ++ "\n\n" ++
            intercalate "\n\n" (show <$> xs)