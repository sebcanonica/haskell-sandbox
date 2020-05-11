import Data.Char

myRemove test [] = []
myRemove test (x:xs) = if test x
                       then myRemove test xs
                       else x : myRemove test xs

myProduct xs = foldl (*) 1 xs

myElem x xs = length (filter (==x) xs) > 0

isPalindromeWord word = word == reverse word

isPalindrome xs = letters == reverse letters
                  where letters = map toLower (filter (/= ' ') xs)

harmonic n = foldl (+) 0 (map (1/) [1..n])

harmonicBook n = sum (take n seriesValues)
  where seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]
        seriesValues = map
                       (\pair -> (fst pair)/(snd pair))
                       seriesPairs