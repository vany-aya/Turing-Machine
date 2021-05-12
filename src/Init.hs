module Init
    (
        getWorld
        ,emptyWorld
    )where

import Data.Either
import Data.Maybe

import Types
import Parsing
import Data.Map as M
import Data.List as L
import Data.Set as S
import Graphics.Gloss

getWorld :: String -> Either World [Error]
getWorld str = getWorld1 (parseConfig str)
 where
 getWorld1 :: Either MT [Error] -> Either World [Error]
 getWorld1 (Right errs) = Right errs
 getWorld1 (Left (Types.MT cs cp st t fc alph)) = Left (World mt 0 (cp * 20) (cell_w * (fromJust (elemIndex (readStripe cp 0 st) alph))) (30 * (fromJust (elemIndex cs r))) r pic (DFSM True (False,' ') False False False) cs st)
  where
  mt = (MT cs cp st t fc alph)
  pic = Pictures (tabl ++ lenta ++ cells ++ col ++ rws)
  lenta = ((Line [(20,height + 35) , (440,height + 35)]) : ((Line [(20,height + 55) , (440,height + 55)]) : ([Line [(x,height + 35) , (x, height + 55)]| x <- [20 , 40 .. 440] ])))
  tabl = ([Line [(x,0) , (x, height) ] | x <- [0 , 70 .. weight]] ++ [Line [(0,x) , (weight,x) ] | x <- [0 , 20 .. height]])
  cells = (L.map (\((st2, ch2) , (st3, (nch2, step2))) -> (Translate (cell_x ch2) (cell_y st2) (Scale 0.08 0.08 (Text (txt step2 nch2 st3))))) (M.toList t))
  col = (L.map (\strr -> (Translate (35 + (-1) * (fromIntegral cell_w)) (cell_y strr) (Scale 0.08 0.08 (Text strr)))) r)
  rws = (L.map (\c -> (Translate (10 + (cell_x c)) (height + 5) (Scale 0.08 0.08 (Text [c])))) alph)
  weight = fromIntegral (cell_w * (length alph))
  cell_w = 70
  height = fromIntegral (20 * (length r))
  cell_x ch2= fromIntegral (4 + cell_w * (fromJust (elemIndex ch2 alph))) 
  cell_y st2= fromIntegral (4 + 20 * (fromJust (elemIndex st2 r)))
  txt c nch2 st3=  (nch2 : (", " ++ st3 ++ (step3 c)) )
  step3 c = case c of 
               0 -> ", 0"
               1 -> ", 1"
               (-1) -> ", -1" 
  r = ((nub (L.map (\x -> (fst x)) (M.keys t))) ++ (nub (S.toList(S.delete cs fc))))

emptyWorld :: World
emptyWorld = (World (MT [] 0 [] (M.empty) (S.empty) []) 0 0 0 0 [] (Pictures []) (DFSM True (False,' ') False False False) [] [])