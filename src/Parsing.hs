module Parsing
    (
      parseConfig
    ) where

import Data.Map as M
import Data.Maybe
import Data.Either
import Data.Set as S
import Data.List as L
import Types

 

parseConfig :: String -> Either MT [Error]
parseConfig str = parseMT (parseTable str)

parseMT :: (Either Table [Error], [State], String) -> Either MT [Error]
parseMT (Right errs, _ , _ ) = Right errs
parseMT (Left t, (x:xs), alph) = Left (MT x 0 (initStripe (stripe (head alph)) [(head alph)]) t (S.fromList xs) alph) 

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
 
 
 
getConds :: [String] -> String -> [Either ((State,Char), (State,(Char,Step))) Error]
getConds [] _ = []
getConds (x:xs) alph = ((getCond x alph (S.fromList alph)) ++ (getConds xs alph))
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
getDataCell2 state ([x] : ([z] : (['0'] : [y]))) w alph = if (x == w ) then (if (S.member z alph) then (Left ((state,x) , (y , (z , 0)))) else (Right (UnknownToLetter state z))) else (Right(ExpectedFromLetter state x))
getDataCell2 state ([x] : ([z] : (('-' : ['1']) : [y]))) w alph = if (x == w ) then (if (S.member z alph) then (Left ((state,x) , (y , (z , -1)))) else (Right(UnknownToLetter state z))) else (Right(ExpectedFromLetter state x))
getDataCell2 state ([x] : ([z] : (['1'] : [y]))) w alph = if (x == w ) then (if (S.member z alph) then (Left ((state,x) , (y , (z , 1)))) else (Right(UnknownToLetter state z))) else (Right(ExpectedFromLetter state x))
getDataCell2 state _ _ _= Right (ParseError state)
getDataCells4 :: String -> String -> String -> Set Char -> [Either ((State,Char) ,(State,(Char,Step))) Error]
getDataCells4 state ('}' : xs) alph alph2 = getDataCells2 state xs alph alph2
getDataCells4 state _ _ _ = [Right (ExpectedRightBr state)]
makeTable :: [Either ((State,Char), (State,(Char,Step))) Error] -> Either Table [Error]
makeTable x = if (length (rights x) > 0) then (Right (rights x)) else  (Prelude.foldl (\acc y -> if (isRight acc) then acc else (if (M.member (fst y) (fromLeft (M.empty) acc)) then (Right [RepeatedStateDeclaration (fst(fst y))]) else (Left (M.insert (fst y)(snd y) (fromLeft (M.empty) acc))))) (Left M.empty) (lefts x))
verifyTable :: String -> [String] -> Either Table [Error] -> Either Table [Error]
verifyTable _ _ (Right errs) = Right errs
verifyTable start finish (Left t) = if (verifyTable1 start (S.fromList finish) t) then (Left t) else (Right [ParseError "Unknown"])
verifyTable1 :: String -> Set String -> Table -> Bool
verifyTable1 start fins t = verifyTable2 start fins (M.foldr (\x acc -> (S.insert (fst x) acc)) (S.empty) t) (M.foldrWithKey (\x _ acc -> (S.insert (fst x) acc)) (S.empty) t)
verifyTable2 :: String -> Set String -> Set String ->Set String -> Bool
verifyTable2 start fins goTo goFrom = ((isSubsetOf goTo (S.union fins goFrom)) && ((S.member start fins) || (S.member start goFrom)))