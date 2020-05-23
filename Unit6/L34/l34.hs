
head :: Monoid a => [a] -> a
head (x:xs) = x
head [] = mempty

toto = head []