import qualified Data.Map as Map

echo :: IO ()
--echo = getLine >>= putStrLn
echo = do
    value <- getLine
    putStrLn value

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
   { candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
   where passedCoding = codeReview candidate > B
         passedCultureFit = cultureFit candidate > C
         educationMin = education candidate >= MS
         tests = [passedCoding
                 ,passedCultureFit
                 ,educationMin]

john = Candidate 1 A B MS
johnFit = viable john

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                           ,(2,candidate2)
                           ,(3,candidate3)]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
--readGrade = getLine >>= (return . read)
readGrade = do
    input <- getLine
    return (read input)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return (Candidate { candidateId = cId
                     , codeReview = codeGrade
                     , cultureFit = cultureGrade
                     , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
   candidate <- readCandidate
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
   candidate <- Map.lookup cId candidateDB
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

printAssessmentResult :: Maybe String -> String
printAssessmentResult (Just s) = s
printAssessmentResult Nothing = "error id not found"

candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

assessCandidate :: Monad m =>  m Candidate -> m String
assessCandidate candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed
                   then "passed"
                   else "failed"
   return statement

------------------------------------------

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
   where costP1 = costPerInch p1
         costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
   where costSqInch = costPerInch (size,cost)


mainPizzaSugar :: IO ()
mainPizzaSugar = do
   putStrLn "What is the size of pizza 1"
   size1 <- getLine
   putStrLn "What is the cost of pizza 1"
   cost1 <- getLine
   putStrLn "What is the size of pizza 2"
   size2 <-  getLine
   putStrLn "What is the cost of pizza 2"
   cost2 <- getLine
   let pizza1 = (read size1, read cost1)
   let pizza2 = (read size2, read cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   putStrLn (describePizza betterPizza)

mainPizza :: IO ()
mainPizza =
    putStrLn "What is the size of pizza 1" >>
    getLine >>= (\size1 ->
        putStrLn "What is the cost of pizza 1" >>
        getLine >>= (\cost1 ->
            putStrLn "What is the size of pizza 2" >>
            getLine >>= (\size2 ->
                putStrLn "What is the cost of pizza 2" >>
                getLine >>= (\cost2 ->
                    (\pizza1 ->
                        (\pizza2 ->
                            putStrLn (describePizza (comparePizzas pizza1 pizza2))
                        ) (read size2, read cost2)
                    ) (read size1, read cost1)
                )
            )
        )
    )

maybeMain :: Maybe String
maybeMain = do
   size1 <- Map.lookup 1 sizeData
   cost1 <- Map.lookup 1 costData
   size2 <- Map.lookup 2 sizeData
   cost2 <- Map.lookup 2 costData
   let pizza1 = (size1,cost1)
   let pizza2 = (size2,cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)

costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

listMain :: [String]
listMain = do
   size1 <- [10,12,17]
   cost1 <- [12.0,15.0,20.0]
   size2 <- [10,11,18]
   cost2 <- [13.0,14.0,21.0]
   let pizza1 = (size1,cost1)
   let pizza2 = (size2,cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)

monadMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
monadMain s1 c1 s2 c2 = do
   size1 <- s1
   cost1 <- c1
   size2 <- s2
   cost2 <- c2
   let pizza1 = (size1,cost1) :: Pizza
   let pizza2 = (size2,cost2) :: Pizza
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)