module Drawer
    (
        renderer

    )where


import Types
import Graphics.Gloss
import Data.List as L


renderer :: World -> Picture
renderer (World (MT cs cp strip t fc alph) frame crp tx ty r (Pictures x) (DFSM _ badCh paus br _) _ _) = Pictures ((cell : (y : x)) ++ drawLetters ++ badChar ++ pausee ++ mode)
 where
 y = Line [(225 + crpp,height + 60) , (225 + crpp + 3, 60 + height - 3) , (225 + crpp + 6, 60 + height), (225 + crpp,60 + height)]
 crpp = fromIntegral crp
 height = fromIntegral (20 * (length r))
 drawLetters :: [Picture]
 drawLetters = L.map (\x -> (Translate (22 + 20 * (fromIntegral (x - frame))) (height + 40) (Scale 0.09 0.09 (Text [readStripe (x - 10) 0 strip])))) [(frame), (frame + 1) .. (frame + 20)]
 cell = Line [((fromIntegral tx) + 4,(fromIntegral ty) + 4) , ((fromIntegral tx) + 4,(fromIntegral ty) + 20 - 4) , ((fromIntegral tx) + 70 - 4,(fromIntegral ty) + 20 - 4) , ((fromIntegral tx)+ 70 - 4,(fromIntegral ty) + 4) , ((fromIntegral tx) + 4,(fromIntegral ty) + 4)]
 badChar = if (fst badCh) then [(Translate 50 255 (Scale 0.4 0.4 (Text ("Bad Char   \'" ++ [snd badCh] ++ ['\'']))))] else []
 pausee = if (paus) then  [(Translate 150 300 (Scale 0.4 0.4 (Text ("Pause. "))))] else []
 mode = if(br) then  [(Translate 0 255 (Scale 0.4 0.4 (Text ("Debug mode. "))))] else []