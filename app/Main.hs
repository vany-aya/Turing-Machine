module Main where

import System.Environment
import Graphics.Gloss
import Data.Either
import Init
import Drawer
import Engine
import Control
  
main :: IO ()
main = do
  args <- getArgs
  str <- readFile (head args)
  let wrld = (getWorld str)
  if (isLeft wrld) then
    play (InWindow "Just window" (400, 400) (100, 100)) (greyN 1) 90 (fromLeft emptyWorld (getWorld str)) renderer handler updater
  else
    print (fromRight [] (wrld))
