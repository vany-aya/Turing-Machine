module Control
    (
        handler
    )where

import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

import Types

handler :: Event -> World -> World
handler (EventKey (SpecialKey KeyF4) Down _ _) (World mt frame crp tx ty r pictrs (DFSM rding badCh paus True True) ist istr) = (World mt frame crp tx ty r pictrs (DFSM rding badCh paus True False) ist istr)
handler (EventKey (SpecialKey KeyF3) Down _ _) (World mt frame crp tx ty r pictrs (DFSM rding badCh paus br stp) ist istr) = (World mt frame crp tx ty r pictrs (DFSM rding badCh paus (not br) False) ist istr)
handler (EventKey (SpecialKey KeyF1) Down _ _) (World (MT cs cp strip t fc alph) frame crp tx ty r (Pictures pic) (DFSM _ badCh _ br stp) ist istr) = (World (MT ist 0 (initStripe (stripe (head alph)) [(head alph)]) t fc alph) 0 0 0 0 r (Pictures pic) (DFSM True badCh False False stp) ist istr)
handler (EventKey (SpecialKey KeyF2) Down _ _) (World (MT cs cp strip t fc alph) frame crp tx ty r (Pictures pic) (DFSM False badCh _ br stp) ist istr) = (World (MT ist 0 istr t fc alph) 0 0 0 0 r (Pictures pic) (DFSM False badCh False False False) ist istr)
handler (EventKey (Char ch) Down _ _)  (World (MT cs cp strip t fc alph) frame crp tx ty r (Pictures pic) (DFSM True badCh False False False)  ist istr) = (World (MT cs cpp strp t fc alph) frm curs tx ty r pict (DFSM True ((not correctChar),ch) False False False)  ist istr)
 where
 frm = if (correctChar) then (if (crp + 20 < 10 * 20) then frame else (5 + frame)) else frame
 curs = if (correctChar) then (if (crp + 20 < 10 * 20) then crp + 20 else crp + 20 - 5 * 20) else crp
 pict = if (correctChar) then (Pictures pic) else (Pictures pic)
 strp = if (correctChar) then (writeStipe strip cp 0 ch) else strip
 cpp = if (correctChar) then (cp + 1) else cp
 correctChar = (S.member ch (S.fromList alph))
handler (EventKey (SpecialKey KeyEnter) Down _ _)  (World (MT cs cp strip t fc alph) frame crp tx ty r pic (DFSM True badCh False br st)  ist istr) =  (World (MT cs 0 strip t fc alph) 0 0 tx ty r pic (DFSM False (False,' ') False br st)  ist strip)
handler (EventKey (SpecialKey KeySpace) Down _ _) (World (MT cs cp strip t fc alph) frame crp tx ty r pic (DFSM False badCh p br st)  ist istr) = (World (MT cs cp strip t fc alph) frame crp tx ty r pic (DFSM False badCh (not p) br st)  ist istr)
handler _ x = x