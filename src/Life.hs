module Life (Cell, Board, step, neighbours) where

type Cell  = (Int, Int)
type Board = [Cell]      -- the cells that are alive

neighbours :: Cell -> [Cell]
neighbours (x, y) = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x - 1, y), (x + 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]

step :: Board -> Board
step board = board       -- placeholder: nothing changes yet