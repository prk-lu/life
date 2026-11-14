module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Life (Board, Cell, step)

-- Starting pattern: a glider. Gloss's y axis points up.
start :: Board
start = [(1, 0), (2, -1), (0, -2), (1, -2), (2, -2)]

data World = World
  { board   :: Board
  , running :: Bool
  , timer   :: Float
  }

initialWorld :: World
initialWorld = World start False 0     -- starts paused

cellSize :: Float
cellSize = 15

secondsPerStep :: Float
secondsPerStep = 0.15

-- Drawing
draw :: World -> Picture
draw world = pictures (map drawCell (board world) ++ [hud])
  where
    drawCell (x, y) =
      translate (fromIntegral x * cellSize) (fromIntegral y * cellSize)
        (color (makeColorI 120 220 140 255) (rectangleSolid (cellSize - 1) (cellSize - 1)))
    hud =
      translate (-390) 280 . scale 0.1 0.1 . color (greyN 0.6) . text $
        (if running world then "running" else "PAUSED")
          ++ "   [space] start/pause  [click] toggle  [n] step  [c] clear  [r] reset"

-- Input
toCell :: (Float, Float) -> Cell
toCell (mx, my) = (round (mx / cellSize), round (my / cellSize))

toggle :: Cell -> Board -> Board
toggle c b
  | c `elem` b = filter (/= c) b
  | otherwise  = c : b

handle :: Event -> World -> World
handle (EventKey (MouseButton LeftButton) Down _ pos) w = w { board = toggle (toCell pos) (board w) }
handle (EventKey (SpecialKey KeySpace) Down _ _) w = w { running = not (running w) }
handle (EventKey (Char 'n') Down _ _) w = w { board = step (board w) }
handle (EventKey (Char 'c') Down _ _) w = w { board = [] }
handle (EventKey (Char 'r') Down _ _) _ = initialWorld
handle _ w = w

-- Time
update :: Float -> World -> World
update dt w
  | not (running w)     = w
  | t >= secondsPerStep = w { board = step (board w), timer = t - secondsPerStep }
  | otherwise           = w { timer = t }
  where t = timer w + dt

main :: IO ()
main = play (InWindow "Game of Life" (800, 600) (100, 100)) black 60 initialWorld draw handle update