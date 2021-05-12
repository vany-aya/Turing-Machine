module Engine
    (
        updater
    )where

import Types
import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Either
import Data.Maybe


updater :: Float -> World -> World
updater _ wrld | ((stp (dfsm wrld)) == True) = wrld
updater _ wrld | ((pause (dfsm wrld)) == True) = wrld
updater _ wrld | ((readingWord (dfsm wrld)) ==  True) = wrld
updater _ (World (MT cs currp str t fs alph) frame cp tx ty r pic fsm ist istr) = if (notNeedsMoving1) then (if (notNeedsMoving2_x) then (if (notNeedsMoving2_y) then (nextMtStep) else (moveTableYCursor world)) else (moveTableXCursor world)) else (mooveStripCursor world)
 where
 mooveStripCursor :: World -> World
 mooveStripCursor (World (MT cs currp str t fc alph) frame cp tx ty r pic fsm ist istr) = if (((currp - frame) * 20 - cp) > 0) then w1 else w2
  where
  w1 = if (cp + 1 < 10 * 20) then (World (MT cs currp str t fc alph) frame (cp + 1) tx ty r pic fsm ist istr) else (World (MT cs currp str t fc alph) (frame + 5) (cp + 1 - 5 * 20) tx ty r pic fsm ist istr)
  w2 = if (cp - 1 > ((-10) * 20)) then (World (MT cs currp str t fc alph) frame (cp - 1) tx ty r pic fsm ist istr) else (World (MT cs currp str t fc alph) (frame - 5) (cp - 1 + 5 * 20) tx ty r pic fsm ist istr)
 moveTableXCursor :: World -> World
 moveTableXCursor (World (MT cs currp str t fc alph) frame cp tx ty r pic fsm ist istr) = if (((fromJust (elemIndex (readStripe currp 0 str) alph)) * cell_w - tx) > 0) then w1 else w2
  where
  w1 = (World (MT cs currp str t fc alph) frame cp (tx + 1) ty r pic fsm ist istr)
  w2 = (World (MT cs currp str t fc alph) frame cp (tx - 1) ty r pic fsm ist istr)
 moveTableYCursor :: World -> World
 moveTableYCursor (World (MT cs currp str t fc alph) frame cp tx ty r pic fsm ist istr) = if (((fromJust (elemIndex cs r)) * 20 - ty) > 0) then w1 else w2
  where
  w1 = (World (MT cs currp str t fc alph) frame cp tx (ty + 1) r pic fsm ist istr)
  w2 = (World (MT cs currp str t fc alph) frame cp tx (ty - 1) r pic fsm ist istr)
 notNeedsMoving1 = ((currp - frame) * 20 - cp == 0)
 notNeedsMoving2_x = (((fromJust (elemIndex (readStripe currp 0 str) alph)) * cell_w - tx) == 0)
 notNeedsMoving2_y = (((fromJust (elemIndex cs r)) * 20 - ty) == 0)
 mt = (MT cs currp str t fs alph)
 cell_w = 70
 world = (World mt frame cp tx ty r pic fsm ist istr)
 nextMtStep = if (isRight (processMT mt)) then world else (World (fromLeft (MT [] 0 [] M.empty S.empty []) (processMT mt)) frame cp tx ty r pic newFsm ist istr)
  where
  newFsm = (DFSM (readingWord fsm) (badCh fsm) (pause fsm) (Types.break fsm) stpp)
  stpp = if ((Types.break fsm) == True) then True else (stp fsm)
 

processMT :: MT -> Either MT TerminatedMT
processMT (MT cs cp st t fss alph) = if (isTerminated) then (Right TerminatedMT) else (Left (MT ns np nst t fss alph))
 where
 ns = fst datacell
 nst = (writeStipe st cp 0 (fst(snd datacell )))
 np = cp + snd(snd datacell)
 datacell = M.findWithDefault ("0",(' ',0)) (cs,(readStripe cp 0 st)) t
 isTerminated = (S.member cs fss)