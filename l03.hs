sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x+y)^2

doubleDoubleOriginal x = dubs*2
        where dubs = x*2        

doubleDouble x = (\d -> d*2) x*2

overwriteOriginal x = let x = 2
              in
               let x = 3
               in
                let x = 4
                in
                 x

--overwrite x = (\x4->4) ((\x3->3) ((\x2->2) x))
overwrite x = (\x -> (\x -> (\x -> x) 4) 3) 2

incL = \n -> n + 1
doubleL = \n -> n * 2
squareL = \n -> n * n

counter x = let x = x + 1
            in
             let x = x + 1
             in
              x

-- counterL x = (\x3 -> ((\x2 -> x2) (x+1)) + 1)
counterL x = (\x -> (\x -> x) (x+1)) (x+1)

calcChange owed given = let change = given - owed
                        in if change > 0 then change else 0    