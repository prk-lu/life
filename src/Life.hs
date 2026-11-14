module Life (Cell, Board, step, neighbours) where

import Data.List (nub)

type Cell  = (Int, Int)
type Board = [Cell]

neighbours :: Cell -> [Cell]
neighbours (x, y) = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x - 1, y), (x + 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]

liveNeighbours :: Board -> Cell -> Int
liveNeighbours board cell = length (filter (`elem` board) (neighbours cell))

aliveNext :: Board -> Cell -> Bool
aliveNext board cell
  | cell `elem` board = n == 2 || n == 3
  | otherwise         = n == 3
  where
    n = liveNeighbours board cell

step :: Board -> Board
step board = filter (aliveNext board) candidates
  where
    candidates = nub (concatMap neighbours board)
