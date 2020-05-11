myLength [] = 0
-- myLength xs = 1 + myLength (tail xs)
myLength (_:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

myCycle xs = xs ++ myCycle xs

ackermann :: Int -> Int -> Int -- optimised for speed
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

collatz 1 = 1
collatz n = if even n 
            then 1+collatz (n `div` 2)
            else 1+collatz (3*n+1)

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- fib n-1 = fib n-2 + fib n-3
-- fib n = 2*fib n-2 + fib n-3

fastFibInternal _ _ 0 = 0
fastFibInternal _ _ 1 = 1
fastFibInternal n2 n1 2 = n2+n1
fastFibInternal n2 n1 count = fastFibInternal n1 (n2+n1) (count-1)
fastFib = fastFibInternal 0 1
