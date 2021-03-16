module Main where


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
 }

getWorld :: String -> Either World [Error]
getWorld str = getWorld1 (parseConfig str)
 where
 getWorld1 :: Either MT [Error] -> Either World [Error]
 getWorld1 (Right errs) = Right errs
 getWorld1 (Left (MT cs cp st t fc alph)) = Left (World mt 0 (cp * 20) (cell_w * (fromJust (elemIndex (readStripe cp 0 st) alph))) (30 * (fromJust (elemIndex cs r))) r pic)
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
handler _ x = x

renderer :: World -> Picture
renderer (World (MT cs cp strip t fc alph) frame crp tx ty r (Pictures x)) = Pictures ((cell : (y : x)) ++ drawLetters)
 where
 y = Line [(225 + crpp,height + 60) , (225 + crpp + 3, 60 + height - 3) , (225 + crpp + 6, 60 + height), (225 + crpp,60 + height)]
 crpp = fromIntegral crp
 height = fromIntegral (20 * (length r))
 drawLetters :: [Picture]
 drawLetters = Data.List.map (\x -> (Translate (22 + 20 * (fromIntegral (x - frame))) (height + 40) (Scale 0.09 0.09 (Text [readStripe (x - 10) 0 strip])))) [(frame), (frame + 1) .. (frame + 20)]
 cell = Line [((fromIntegral tx) + 4,(fromIntegral ty) + 4) , ((fromIntegral tx) + 4,(fromIntegral ty) + 20 - 4) , ((fromIntegral tx) + 70 - 4,(fromIntegral ty) + 20 - 4) , ((fromIntegral tx)+ 70 - 4,(fromIntegral ty) + 4) , ((fromIntegral tx) + 4,(fromIntegral ty) + 4)]
 
updater :: Float -> World -> World
updater _ (World (MT cs currp str t fs alph) frame cp tx ty r pic) = if (notNeedsMoving1) then (if (notNeedsMoving2_x) then (if (notNeedsMoving2_y) then (nextMtStep) else (moveTableYCursor world)) else (moveTableXCursor world)) else (mooveStripCursor world)
 where
 mooveStripCursor :: World -> World
 mooveStripCursor (World (MT cs currp str t fc alph) frame cp tx ty r pic) = if (((currp - frame) * 20 - cp) > 0) then w1 else w2
  where
  w1 = if (cp + 1 < 10 * 20) then (World (MT cs currp str t fc alph) frame (cp + 1) tx ty r pic) else (World (MT cs currp str t fc alph) (frame + 5) (cp + 1 - 5 * 20) tx ty r pic)
  w2 = if (cp - 1 > ((-10) * 20)) then (World (MT cs currp str t fc alph) frame (cp - 1) tx ty r pic) else (World (MT cs currp str t fc alph) (frame - 5) (cp - 1 + 5 * 20) tx ty r pic)
 moveTableXCursor :: World -> World
 moveTableXCursor (World (MT cs currp str t fc alph) frame cp tx ty r pic) = if (((fromJust (elemIndex (readStripe currp 0 str) alph)) * cell_w - tx) > 0) then w1 else w2
  where
  w1 = (World (MT cs currp str t fc alph) frame cp (tx + 1) ty r pic)
  w2 = (World (MT cs currp str t fc alph) frame cp (tx - 1) ty r pic)
 moveTableYCursor :: World -> World
 moveTableYCursor (World (MT cs currp str t fc alph) frame cp tx ty r pic) = if (((fromJust (elemIndex cs r)) * 20 - ty) > 0) then w1 else w2
  where
  w1 = (World (MT cs currp str t fc alph) frame cp tx (ty + 1) r pic)
  w2 = (World (MT cs currp str t fc alph) frame cp tx (ty - 1) r pic)
 notNeedsMoving1 = ((currp - frame) * 20 - cp == 0)
 notNeedsMoving2_x = (((fromJust (elemIndex (readStripe currp 0 str) alph)) * cell_w - tx) == 0)
 notNeedsMoving2_y = (((fromJust (elemIndex cs r)) * 20 - ty) == 0)
 mt = (MT cs currp str t fs alph)
 cell_w = 70
 world = (World mt frame cp tx ty r pic)
 nextMtStep = if (isRight (processMT mt)) then world else (World (fromLeft (MT [] 0 [] Data.Map.empty Data.Set.empty []) (processMT mt)) frame cp tx ty r pic)
 

initStripe :: Stripe -> String -> Stripe
initStripe stripe str = Prelude.foldl (\acc (x , y) -> (writeStipe acc y 0 x)) stripe (Data.List.zip str [0, 1 .. ((length str) - 1)]) 

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
parseMT (Left t, (x:xs), alph) = Left (MT x 0 (initStripe (stripe (head alph)) "aaabbbcccabc") t (Data.Set.fromList xs) alph) 

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
main = play (InWindow "Nice Window" (400, 400) (100, 100)) (greyN 1) 90 (fromLeft (World (MT [] 0 [] (Data.Map.empty) (Data.Set.empty) []) 0 0 0 0 [] (Pictures [])) (getWorld "*abc^\n  s1 fin\n s1 {* * -1 s2 } {a a -1 s2}  {b b -1 s2} {c c -1 s2 } {^ ^ -1 s2} \n s2 {* ^ 1 s3} {a b 1 s2} {b b -1 s2  } {c c -1 s2} {^ a -1 s2}  \n s3 {* * -1 s4} {a a 1 s3} {b b 1 s3} {c c 1 s3} {^ ^ 1 s8} \n s4 {* * 0 s4} {a * -1 s5} {b * -1 s6} {c * -1 s7} {^ ^ 0 s4} \n s5 {* a 1 s3} {a a -1 s5} {b b -1 s5} {c c -1 s5} {^ ^ -1 s5}\n s6 {* b 1 s3} {a a -1 s6} {b b -1 s6} {c c -1 s6} {^ ^ -1 s6}\n s7 {* c 1 s3} {a a -1 s7} {b b -1 s7} {c c -1 s7} {^ ^ -1 s7}\n s8 {* * 0 fin} {a a 0 s3} {b b 0 s3} {c c 0 s3} {^ ^ 0 s3} ")) renderer handler updater
--main = print (parseConfig "abcde\n  start fin1 qaa fin2\n start {a  c  -1  med  }  {b  b 0 fin2}  {c a -1 med  }  {d e 0 start  } {e a 1 start  } \n med { a b 1 start} {b a 1 start  } {c b -1 qaa} {d c -1 med} {e c -1 med}  \n   ")
