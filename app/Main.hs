module Main where

import Life (step, neighbours)

main :: IO ()
main = do
  print (neighbours (0, 0))
  let blinker = [(0, 1), (1, 1), (2, 1)]
  print blinker
  print (step blinker)