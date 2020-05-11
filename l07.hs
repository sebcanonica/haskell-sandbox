sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

isEmpty [] = True
isEmpty _ = False
myHead (x:xs) = x -- convention x=single value, xs=list
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = []

myGCDOriginal a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

