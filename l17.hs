import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = foldr (||) False . map testFunc

howMuch :: Int -> String
howMuch n | n > 10 = "a whole bunch"
          | n > 0 = "not much"
          | otherwise = "we're in debt!"

data Color =
   White |
   Red |
   Yellow |
   Blue |
   Green |
   Purple |
   Orange |
   Brown deriving (Show,Eq)

instance Semigroup Color where
   (<>) White a = a
   (<>) a White = a
   (<>) Red Blue = Purple
   (<>) Blue Red = Purple
   (<>) Yellow Blue = Green
   (<>) Blue Yellow = Green
   (<>) Yellow Red = Orange
   (<>) Red Yellow = Orange
   (<>) a b | a == b = a
            | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
            | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
            | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
            | otherwise = Brown

instance Monoid Color where
   mempty = White
   mappend = (<>)

newtype Events = Events [String] deriving Show

instance Semigroup Events where
   (<>) (Events []) e = e
   (<>) e (Events []) = e
   (<>) (Events names1) (Events names2) = Events (cartCombine combiner names1 names2)
         where combiner x y = mconcat [x,"-",y]

instance Monoid Events where
   mempty = Events []
   mappend = (<>)

newtype Probs = Probs [Double] deriving Show

instance Semigroup Probs where
   (<>) (Probs probs1) (Probs probs2) = Probs (cartCombine (*) probs1 probs2)

instance Monoid Probs where
   mempty = Probs [1]
   mappend = (<>)

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
 where nToAdd = length l2
       repeatedL1 = map (take nToAdd . repeat) l1
       newL1 = mconcat repeatedL1
       cycledL2 = cycle l2

coinEvent = Events ["head","tail"]
coinProbs = Probs [0.5,0.5]
spinEvent = Events ["red", "green", "blue"]
spinProbs = Probs [1/3,1/3,1/3]

{-combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
 where combiner = \x y -> mconcat [x,"-",y]

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2-}