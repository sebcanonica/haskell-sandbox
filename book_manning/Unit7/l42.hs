import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) [] -- (startI,endI) [(idx,val)]

qcArray :: UArray Int Bool
qcArray = array (0,4) [(1,True),(2,True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) $ zip [0..3] $ repeat 0
updatedBiB = beansInBuckets // [(1,5),(3,6)] -- set values in a copy
accumBiB = accum (+) updatedBiB $ zip [0 .. 3] $ repeat 2 -- apply an operation

doubleBiB = accum (*) updatedBiB $ zip [0..3] $ repeat 2

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
      let val = vals !! i
      writeArray myArray i val
    return myArray

listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ listToSTUArray vals

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]
myData' = listToUArray [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
   stArray <- thaw myArray
   let end = (snd . bounds) myArray
   forM_ [1 .. end] $ \i -> do
     forM_ [0 .. (end - i)] $ \j -> do
       val <- readArray stArray j
       nextVal <- readArray stArray (j + 1)
       let outOfOrder = val > nextVal
       when outOfOrder $ do
         writeArray stArray j nextVal
         writeArray stArray (j + 1) val
   return stArray

sortedData = bubbleSort myData

crossover :: Int -> (UArray Int Bool, UArray Int Bool) -> UArray Int Bool
crossover cutoff (a1, a2) = runSTUArray $ do
    stA1 <- thaw a1
    let upperBound = snd . bounds $ a2
    forM_ [cutoff..upperBound] $ \i -> do
        let val = a2 ! i
        writeArray stA1 i val
    return stA1

allTrue = listArray (0,4) $ repeat True
allFalse = listArray (0,4) $ repeat False
crossed = crossover 3 (allTrue,allFalse)

replaceZeroes :: UArray Int Int -> UArray Int Int
replaceZeroes a = runSTUArray $ do
    stA <- thaw a
    let start = fst . bounds $ a
    let end = snd . bounds $ a
    forM_ [start..end] $ \i -> do
        val <- readArray stA i
        when (i == 0) $
            writeArray stA i (-1)
    return stA

mixedValue = listArray (0,5) [0,1,2,3,0,4]
replacedValue = replaceZeroes mixedValue
