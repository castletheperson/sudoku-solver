module Sudoku
    ( Sudoku
    , Cell(..)
    , solveSudoku
    ) where

import Control.Monad
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Array
import Data.Foldable
import Data.Ord

data Cell
    = Known Int
    | Unknown [Int]
    deriving (Read, Show, Eq)

type Sudoku = [[Cell]]
type Board = Array (Int, Int) Cell
type NearbyCells = [[Cell]]

solveSudoku :: Sudoku -> [Sudoku]
solveSudoku = (fromArray <$>) . solveBoard . toArray
    where
        fromArray board = chunksOf 9 (elems board)
        toArray board = array ((0,0),(8,8))
            [((row, col), cell) |
                (row, cells) <- zip [0..8] board,
                (col, cell)  <- zip [0..8] cells]

solveBoard :: Board -> [Board]
solveBoard = simplify >=> \board ->
    if all isKnown board
        then return board
        else guess board >>= solveBoard

simplify :: Board -> [Board]
simplify board = do
    board' <- toList $ (board //) <$> mapM update (assocs board)
    if board == board'
        then return board'
        else simplify board'
    where
        update ((x, y), cell) = ((,) (x, y) <$>) $
            simplifyObvious nearbyCells cell >>=
            simplifyOnlyOption nearbyCells
            where
                nearbyCells = [
                    get (0, y) (8, y),             -- horizontal
                    get (x, 0) (x, 8),             -- vertical
                    get (x', y') (x' + 2, y' + 2)] -- square
                get from to = (board !) <$> delete (x, y) (range (from, to))
                (x', y') =
                    let flr n = n `quot` 3 * 3
                    in (flr x, flr y)

simplifyObvious :: NearbyCells -> Cell -> Maybe Cell
simplifyObvious nearbyCells cell =
    case cell of
        Known x    -> toCell ([x] \\ ys)
        Unknown xs -> toCell (xs  \\ ys)
    where
        ys = [y | Known y <- join nearbyCells]
        toCell []  = empty
        toCell [x] = pure (Known x)
        toCell xs  = pure (Unknown xs)

simplifyOnlyOption :: NearbyCells -> Cell -> Maybe Cell
simplifyOnlyOption _           (Known x)    = pure (Known x)
simplifyOnlyOption nearbyCells (Unknown xs) =
    asum (trySimplify <$> nearbyCells) <|> pure (Unknown xs)
    where
        trySimplify cells =
            toCell (xs \\ [x | Unknown xs' <- cells, x <- xs'])
        toCell [x] = pure (Known x)
        toCell _   = empty

guess :: Board -> [Board]
guess board = (\opt -> board // [(idx, Known opt)]) <$> opts
    where
        (idx, Unknown opts) =
            minimumBy (comparing (numOptions . snd)) unknowns
        unknowns = filter (not . isKnown . snd) (assocs board)
        numOptions (Unknown xs) = length xs
        numOptions (Known _)    = error "Known values don't need guessed"

isKnown :: Cell -> Bool
isKnown (Known _) = True
isKnown _         = False
