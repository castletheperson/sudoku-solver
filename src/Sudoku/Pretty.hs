module Sudoku.Pretty
    ( PrettySudoku
    , solve
    ) where

import Sudoku
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import Data.List.Split (chunksOf)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

newtype PrettySudoku = PrettySudoku { getBoard :: Sudoku }

instance Show PrettySudoku where
    show = prettyPrintSudoku

instance Read PrettySudoku where
    readsPrec = parsecToReadsPrec (PrettySudoku <$> sudokuParser)

solve :: PrettySudoku -> [PrettySudoku]
solve = (PrettySudoku <$>) . solveSudoku . getBoard

prettyPrintSudoku :: PrettySudoku -> String
prettyPrintSudoku = printBoard . getBoard
    where
        printBoard board = unlines . join .
            intersperse ["--------+---------+--------"] $
            intersperse  "        |         |        " <$>
            chunksOf 3 (printRow <$> board)
        printRow r =
            join . intersperse " | " $
            join . intersperse "  " <$>
            chunksOf 3 (printCell <$> r)
        printCell c = return $
            case c of
                Known x -> intToDigit x
                _       -> '.'

sudokuParser :: Parser Sudoku
sudokuParser = try sudokuCompact <|> sudokuPretty

sudokuCompact :: Parser Sudoku
sudokuCompact = count 9 (count 9 (parseCell (noneOf "\n")) <* endOfLine) <* eof

sudokuPretty :: Parser Sudoku
sudokuPretty = join <$>
    sepByCount 3 (sepByCount 3 (row <* endOfLine)
        (string "        |         |        " <* endOfLine))
        (string "--------+---------+--------" <* endOfLine)
    where
        row = join <$>
            sepByCount 3 (sepByCount 3 (parseCell (char '.'))
                (string "  "))
                (string " | ")

parseCell :: Parser Char -> Parser Cell
parseCell unknownParser =
    Known . digitToInt <$> oneOf ['1'..'9'] <|>
    Unknown [1..9]     <$  unknownParser

-- Utility Functions
sepByCount :: Int -> Parser a -> Parser b -> Parser [a]
sepByCount n p sep = sequence (take n (p : repeat (sep >> p)))

parsecToReadsPrec :: Parser a -> Int -> ReadS a
parsecToReadsPrec parsecParser _ input =
    return . either (error . show) id $
    parse (liftM2 (,) parsecParser getInput) "" input