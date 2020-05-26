myRepeat x = cycle [x]

subseq start end list = take count (drop start list)
                        where count = end - start

inFirstHalf e l = e `elem` firstHalf
                  where 
                      halfLen = length l `div` 2
                      firstHalf = take halfLen l