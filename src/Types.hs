module Types
    (  MT(MT) , 
       TerminatedMT(TerminatedMT),
       DFSM(DFSM),
       Error(NoStateDescribtion,
            NoStartStateDescribtion,
            RepitedStateDescribtion,
            NoLetterDescribtion,
            ExtraLetterDescribtion,
            NoStartStateDeclaration,
            NoFinStatesDeclaration,
            ExpectedLeftBr,
            ExpectedFromLetter,
            UnknownToLetter,
            ParseError,
            ExpectedRightBr,
            RepeatedStateDeclaration,
            NoStartFinStatesDeclarations,
            NoAlph
        ),
       stripe,
       World(World),
       State,
       Step,
       Stripe,
       Table,
       mt
      ,frame
      ,currPos 
      ,tableXPOS 
      ,tableYPOS 
      ,rows 
      ,image 
      ,dfsm 
      ,start 
      ,str 
      ,readingWord
      ,badCh
      ,pause
      ,Types.break
      ,stp
      ,currstate
      ,currpos
      ,strip
      ,table
      ,finConds
      ,alph
      ,initStripe
      ,writeStipe
      ,readStripe
    ) where
import Data.Map
import Data.Set
import Data.List as L

import Graphics.Gloss
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

initStripe :: Stripe -> String -> Stripe
initStripe stripe str = Prelude.foldl (\acc (x , y) -> writeStipe acc y 0 x) stripe (L.zip str [0, 1 .. (length str - 1)])

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