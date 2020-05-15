import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = foldr (||) False . map testFunc

