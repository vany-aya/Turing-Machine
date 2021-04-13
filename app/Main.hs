module Main where

import System.Environment
import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.Maybe
import Data.Either
import Data.Set
import Data.List

type State = String
type Step = Int
type Stripe = [(Char, Char)]
type Table = Map (State,Char) (State,(Char,Step))
data MT = MT
 {  
    currstate :: State
    ,currpos :: Int
    ,strip :: Stripe
    ,table :: Table
    ,finConds :: Set State
    ,alph :: String
 }
  deriving (Show)
data TerminatedMT = TerminatedMT
 deriving (Show)

data DFSM = DFSM
  {
    readingWord :: Bool
    ,badCh :: (Bool, Char)
    ,pause :: Bool
    ,break :: Bool
    ,stp :: Bool
  }
data Error = NoStateDescribtion
           | NoStartStateDescribtion
           | RepitedStateDescribtion State
           | NoLetterDescribtion State String
           | ExtraLetterDescribtion State
           | NoStartStateDeclaration
           | NoFinStatesDeclaration
           | ExpectedLeftBr State
           | ExpectedFromLetter State Char
           | UnknownToLetter State Char
           | ParseError State
           | ExpectedRightBr State
           | RepeatedStateDeclaration State
           | NoStartFinStatesDeclarations
           | NoAlph
            deriving (Show)


stripe :: Char -> [(Char, Char)]
stripe x = ((x, x) : (stripe x))

data World = World
 {
   mt :: MT
   ,frame :: Int
   ,currPos :: Int
   ,tableXPOS :: Int
   ,tableYPOS :: Int
   ,rows :: [String]
   ,image :: Picture
   ,dfsm :: DFSM
   ,start :: State
   ,str :: Stripe
 }

initStripe :: Stripe -> String -> Stripe
initStripe stripe str = Prelude.foldl (\acc (x , y) -> (writeStipe acc y 0 x)) stripe (Data.List.zip str [0, 1 .. ((length str) - 1)]) 

getWorld :: String -> Either World [Error]
getWorld str = getWorld1 (parseConfig str)
 where
 getWorld1 :: Either MT [Error] -> Either World [Error]
 getWorld1 (Right errs) = Right errs
 getWorld1 (Left (MT cs cp st t fc alph)) = Left (World mt 0 (cp * 20) (cell_w * (fromJust (elemIndex (readStripe cp 0 st) alph))) (30 * (fromJust (elemIndex cs r))) r pic (DFSM True (False,' ') False False False) cs st)
  where
  mt = (MT cs cp st t fc alph)
  pic = Pictures (tabl ++ lenta ++ cells ++ col ++ rws)
  lenta = ((Line [(20,height + 35) , (440,height + 35)]) : ((Line [(20,height + 55) , (440,height + 55)]) : ([Line [(x,height + 35) , (x, height + 55)]| x <- [20 , 40 .. 440] ])))
  tabl = ([Line [(x,0) , (x, height) ] | x <- [0 , 70 .. weight]] ++ [Line [(0,x) , (weight,x) ] | x <- [0 , 20 .. height]])
  cells = (Data.List.map (\((st2, ch2) , (st3, (nch2, step2))) -> (Translate (cell_x ch2) (cell_y st2) (Scale 0.08 0.08 (Text (txt step2 nch2 st3))))) (Data.Map.toList t))
  col = (Data.List.map (\strr -> (Translate (35 + (-1) * (fromIntegral cell_w)) (cell_y strr) (Scale 0.08 0.08 (Text strr)))) r)
  rws = (Data.List.map (\c -> (Translate (10 + (cell_x c)) (height + 5) (Scale 0.08 0.08 (Text [c])))) alph)
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
  r = ((nub (Data.List.map (\x -> (fst x)) (Data.Map.keys t))) ++ (nub (Data.Set.toList(Data.Set.delete cs fc))))

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
 correctChar = (Data.Set.member ch (Data.Set.fromList alph))
handler (EventKey (SpecialKey KeyEnter) Down _ _)  (World (MT cs cp strip t fc alph) frame crp tx ty r pic (DFSM True badCh False br st)  ist istr) =  (World (MT cs 0 strip t fc alph) 0 0 tx ty r pic (DFSM False (False,' ') False br st)  ist strip)
handler (EventKey (SpecialKey KeySpace) Down _ _) (World (MT cs cp strip t fc alph) frame crp tx ty r pic (DFSM False badCh p br st)  ist istr) = (World (MT cs cp strip t fc alph) frame crp tx ty r pic (DFSM False badCh (not p) br st)  ist istr)
handler _ x = x
 

renderer :: World -> Picture
renderer (World (MT cs cp strip t fc alph) frame crp tx ty r (Pictures x) (DFSM _ badCh paus br _) _ _) = Pictures ((cell : (y : x)) ++ drawLetters ++ badChar ++ pausee ++ mode)
 where
 y = Line [(225 + crpp,height + 60) , (225 + crpp + 3, 60 + height - 3) , (225 + crpp + 6, 60 + height), (225 + crpp,60 + height)]
 crpp = fromIntegral crp
 height = fromIntegral (20 * (length r))
 drawLetters :: [Picture]
 drawLetters = Data.List.map (\x -> (Translate (22 + 20 * (fromIntegral (x - frame))) (height + 40) (Scale 0.09 0.09 (Text [readStripe (x - 10) 0 strip])))) [(frame), (frame + 1) .. (frame + 20)]
 cell = Line [((fromIntegral tx) + 4,(fromIntegral ty) + 4) , ((fromIntegral tx) + 4,(fromIntegral ty) + 20 - 4) , ((fromIntegral tx) + 70 - 4,(fromIntegral ty) + 20 - 4) , ((fromIntegral tx)+ 70 - 4,(fromIntegral ty) + 4) , ((fromIntegral tx) + 4,(fromIntegral ty) + 4)]
 badChar = if (fst badCh) then [(Translate 50 255 (Scale 0.4 0.4 (Text ("Bad Char   \'" ++ [snd badCh] ++ ['\'']))))] else []
 pausee = if (paus) then  [(Translate 150 300 (Scale 0.4 0.4 (Text ("Pause. "))))] else []
 mode = if(br) then  [(Translate 0 255 (Scale 0.4 0.4 (Text ("Debug mode. "))))] else []
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
 nextMtStep = if (isRight (processMT mt)) then world else (World (fromLeft (MT [] 0 [] Data.Map.empty Data.Set.empty []) (processMT mt)) frame cp tx ty r pic newFsm ist istr)
  where
  newFsm = (DFSM (readingWord fsm) (badCh fsm) (pause fsm) (Main.break fsm) stpp)
  stpp = if ((Main.break fsm) == True) then True else (stp fsm)
 



writeStipe :: Stripe -> Int -> Int -> Char -> Stripe
writeStipe ((x1,x2): xs) x y z | (x == y) && x >= 0 = ((z,x2):xs)
                               | (x == y) && x < 0  = ((x1,z):xs)
                               | x >= 0 = (x1,x2): writeStipe xs x (y + 1) z
                               | x < 0  = (x1,x2): writeStipe xs x (y - 1) z                   

readStripe :: Int -> Int -> Stripe -> Char
readStripe x y ((x1,x2): xs)   | (x == y) && x >= 0 = x1
                               | (x == y) && x < 0  = x2
                               | x >= 0 = readStripe x (y + 1) xs
                               | x < 0  = readStripe x (y - 1) xs

processMT :: MT -> Either MT TerminatedMT
processMT (MT cs cp st t fss alph) = if (isTerminated) then (Right TerminatedMT) else (Left (MT ns np nst t fss alph))
 where
 ns = fst datacell
 nst = (writeStipe st cp 0 (fst(snd datacell )))
 np = cp + snd(snd datacell)
 datacell = Data.Map.findWithDefault ("0",(' ',0)) (cs,(readStripe cp 0 st)) t
 isTerminated = (Data.Set.member cs fss)


parseConfig :: String -> Either MT [Error]
parseConfig str = parseMT (parseTable str)

parseMT :: (Either Table [Error], [State], String) -> Either MT [Error]
parseMT (Right errs, _ , _ ) = Right errs
parseMT (Left t, (x:xs), alph) = Left (MT x 0 (initStripe (stripe (head alph)) [(head alph)]) t (Data.Set.fromList xs) alph) 

parseTable :: String -> (Either Table [Error], [State], String)
parseTable str  |(length (lines str) < 1) = (Right [NoAlph],[],[])
                |(length (lines str) < 2) = (Right [NoStartFinStatesDeclarations],[],[])
                | ((length (words(head(tail (lines str))))) < 1) = (Right [NoStartStateDeclaration],[],[])
                | ((length (words(head(tail (lines str))))) < 2) = (Right [NoFinStatesDeclaration],[],[])
                | otherwise = ((verifyTable startCond finConds (makeTable (getConds otherConds getAlph))),(startCond : finConds), getAlph)
 where
 getAlph  = head (lines str)
 startCond = head (words(head(tail (lines str))))
 finConds = tail (words(head(tail (lines str))))
 otherConds = tail (tail(lines str))
 --getNumConds = read (head(tail(lines str))) :: Int
 
 
 
getConds :: [String] -> String -> [Either ((State,Char), (State,(Char,Step))) Error]
getConds [] _ = []
getConds (x:xs) alph = ((getCond x alph (Data.Set.fromList alph)) ++ (getConds xs alph))
getCond :: String -> String -> Set Char ->[Either ((State,Char), (State,(Char,Step))) Error]
getCond str alph alph2 = if (not((words str) == [])) then (getDataCells2 (head (words str)) (unwords (tail (words str))) alph alph2) else []
getDataCells2 :: State -> String -> String ->Set Char -> [Either ((State,Char), (State,(Char,Step))) Error]
getDataCells2 state ('{' : xs) alph alph2 = if (not (alph == [])) then (getDataCells3 state xs alph alph2) else [Right (ExtraLetterDescribtion state)]
getDataCells2 state (' ' : xs) alph alph2 = getDataCells2 state xs alph alph2
getDataCells2 state [] alph _ = if (alph == [] ) then [] else [Right (NoLetterDescribtion state alph)]
getDataCells2 state _ _ _= [Right (ExpectedLeftBr state)]
getDataCells3 :: State -> String -> String -> Set Char -> [Either ((State,Char), (State,(Char,Step))) Error]
getDataCells3 state str alph alph2 = ((getDataCell state (takeWhile (\x -> not(x == '}')) str) (head alph) alph2) : getDataCells4 state (dropWhile (\x -> (not(x == '}'))) str) (tail alph) alph2)
getDataCell :: String -> String -> Char -> Set Char -> Either ((State,Char), (State,(Char,Step))) Error
getDataCell state str letter alph2 = getDataCell2 state (words str) letter alph2
getDataCell2 :: String -> [String] -> Char -> Set Char-> Either ((State,Char), (State,(Char,Step))) Error
getDataCell2 state ([x] : ([z] : (['0'] : [y]))) w alph = if (x == w ) then (if (Data.Set.member z alph) then (Left ((state,x) , (y , (z , 0)))) else (Right (UnknownToLetter state z))) else (Right(ExpectedFromLetter state x))
getDataCell2 state ([x] : ([z] : (('-' : ['1']) : [y]))) w alph = if (x == w ) then (if (Data.Set.member z alph) then (Left ((state,x) , (y , (z , -1)))) else (Right(UnknownToLetter state z))) else (Right(ExpectedFromLetter state x))
getDataCell2 state ([x] : ([z] : (['1'] : [y]))) w alph = if (x == w ) then (if (Data.Set.member z alph) then (Left ((state,x) , (y , (z , 1)))) else (Right(UnknownToLetter state z))) else (Right(ExpectedFromLetter state x))
getDataCell2 state _ _ _= Right (ParseError state)
getDataCells4 :: String -> String -> String -> Set Char -> [Either ((State,Char) ,(State,(Char,Step))) Error]
getDataCells4 state ('}' : xs) alph alph2 = getDataCells2 state xs alph alph2
getDataCells4 state _ _ _ = [Right (ExpectedRightBr state)]
makeTable :: [Either ((State,Char), (State,(Char,Step))) Error] -> Either Table [Error]
makeTable x = if (length (rights x) > 0) then (Right (rights x)) else  (Prelude.foldl (\acc y -> if (isRight acc) then acc else (if (Data.Map.member (fst y) (fromLeft (Data.Map.empty) acc)) then (Right [RepeatedStateDeclaration (fst(fst y))]) else (Left (Data.Map.insert (fst y)(snd y) (fromLeft (Data.Map.empty) acc))))) (Left Data.Map.empty) (lefts x))
verifyTable :: String -> [String] -> Either Table [Error] -> Either Table [Error]
verifyTable _ _ (Right errs) = Right errs
verifyTable start finish (Left t) = if (verifyTable1 start (Data.Set.fromList finish) t) then (Left t) else (Right [ParseError "Unknown"])
verifyTable1 :: String -> Set String -> Table -> Bool
verifyTable1 start fins t = verifyTable2 start fins (Data.Map.foldr (\x acc -> (Data.Set.insert (fst x) acc)) (Data.Set.empty) t) (Data.Map.foldrWithKey (\x _ acc -> (Data.Set.insert (fst x) acc)) (Data.Set.empty) t)
verifyTable2 :: String -> Set String -> Set String ->Set String -> Bool
verifyTable2 start fins goTo goFrom = ((isSubsetOf goTo (Data.Set.union fins goFrom)) && ((Data.Set.member start fins) || (Data.Set.member start goFrom)))



  


  
  
main :: IO ()
--main = someFunc
--main = display (InWindow "Nice Window" (400, 400) (100, 100)) white (Circle 80)
main = do
  args <- getArgs
  str <- readFile (head args)
  let wrld = (getWorld str)
  if (isLeft wrld) then
    play (InWindow "Nastya is the best of the VMK" (400, 400) (100, 100)) (greyN 1) 90 (fromLeft (World (MT [] 0 [] (Data.Map.empty) (Data.Set.empty) []) 0 0 0 0 [] (Pictures []) (DFSM True (False,' ') False False False) [] []) (getWorld str)) renderer handler updater
  else
    print (fromRight [] (wrld))
--main = print (parseConfig "abcde\n  start fin1 qaa fin2\n start {a  c  -1  med  }  {b  b 0 fin2}  {c a -1 med  }  {d e 0 start  } {e a 1 start  } \n med { a b 1 start} {b a 1 start  } {c b -1 qaa} {d c -1 med} {e c -1 med}  \n   ")
