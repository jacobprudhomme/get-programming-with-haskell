#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import qualified Data.Map as Map


askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName =
  askForName >>
  getLine >>=
  (\name ->
    return (nameStatement name)) >>=
  putStrLn

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM m = m >>= (\(v1,v2) -> return (max v1 v2))

helloNameDo :: IO ()
helloNameDo = do
  askForName
  name <- getLine
  putStrLn $ nameStatement name

helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

main' :: IO ()
main' = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

mainNoDo' :: IO ()
mainNoDo' =
  getLine >>=
  (\name ->
    (\statement ->
      putStrLn statement)
    (helloPerson name))

-- An example of where it's easier to NOT use do-notation
echo :: IO ()
echo = getLine >>= putStrLn

echo' :: IO ()
echo' = do
  input <- getLine
  putStrLn input

data Grade = F | D | C | B | A deriving (Enum, Eq, Ord, Read, Show)

data Degree = HS | BA | MS | PhD deriving (Enum, Eq, Ord, Read, Show)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview  :: Grade
  , cultureFit  :: Grade
  , education   :: Degree
  } deriving Show

viable :: Candidate -> Bool
viable candidate = and tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

exampleCandidate :: Candidate
exampleCandidate = Candidate
  { candidateId = 4
  , codeReview  = A
  , cultureFit  = D
  , education   = MS
  }

isExampleViable = viable exampleCandidate

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "Enter ID:"
  cId <- readInt
  putStrLn "Enter coding grade:"
  codingGrade <- readGrade
  putStrLn "Enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "Enter level of education:"
  degree <- readDegree
  return $ Candidate
    { candidateId = cId
    , codeReview  = codingGrade
    , cultureFit  = cultureGrade
    , education   = degree
    }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let verdict = if passed then "Passed" else "Failed"
  return verdict

readGrade' :: IO Grade
readGrade' = do
  input <- getLine
  return $ read input

candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview  = A
  , cultureFit  = A
  , education   = BA
  }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview  = C
  , cultureFit  = A
  , education   = PhD
  }

candidate3 :: Candidate
candidate3 = Candidate
  { candidateId = 3
  , codeReview  = A
  , cultureFit  = B
  , education   = MS
  }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList
  [ (1, candidate1)
  , (2, candidate2)
  , (3, candidate3)
  ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let verdict = if passed then "Passed" else "Failed"
  return verdict

maybeAssessExample1 = assessCandidateMaybe 1
maybeAssessExample2 = assessCandidateMaybe 3
maybeAssessExample3 = assessCandidateMaybe 4

qc :: Maybe String -> String
qc Nothing        = "Error: ID not found"
qc (Just verdict) = verdict

candidates :: [Candidate]
candidates =
  [ candidate1
  , candidate2
  , candidate3
  ]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let verdict = if passed then "Passed" else "Failed"
  return verdict

listAssessExample1 = assessCandidateList candidates
listAssessExample2 = assessCandidateList []

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates = map (\x ->
  if x
  then "Passed"
  else "Failed")
  passed
  where passed = map viable candidates

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let verdict = if passed then "Passed" else "Failed"
  return verdict

genericExample1 = assessCandidate readCandidate
genericExample2 = assessCandidate $ Map.lookup 1 candidateDB
genericExample3 = assessCandidate candidates

areaFromDiameter :: Double -> Double
areaFromDiameter diam = pi * (diam / 2)^2

type Pizza = (Double,Double)

costPerSqrInch :: Pizza -> Double
costPerSqrInch (diam, price) = price / size
  where size = areaFromDiameter diam

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
  then p1
  else p2
  where
    costP1 = costPerSqrInch p1
    costP2 = costPerSqrInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = mconcat
  [ "The "
  , show size
  , " pizza is cheaper at "
  , show costPerArea
  , " per square inch"
  ]
  where costPerArea = costPerSqrInch (size, cost)

main'' :: IO ()
main'' = do
  putStrLn "What is the size of pizza 1?"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1?"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2?"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2?"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn $ describePizza betterPizza

mainNoDo'' :: IO ()
mainNoDo'' =
  putStrLn "What is the size of pizza 1?" >>
  getLine >>=
  (\size1 ->
    putStrLn "What is the cost of pizza 1?" >>
    getLine >>=
    (\cost1 ->
      putStrLn "What is the size of pizza 2?" >>
      getLine >>=
      (\size2 ->
        putStrLn "What is the cost of pizza 2?" >>
        getLine >>=
        (\cost2 ->
          (\pizza1 ->
            (\pizza2 ->
              (\betterPizza ->
                putStrLn (describePizza betterPizza))
              (comparePizzas pizza1 pizza2))
            (read size2, read cost2))
          (read size1, read cost1)))))

costData :: [Double]
costData = [18.0, 16.0]

sizeData :: [Double]
sizeData = [20.0, 15.0]

listMain :: [String]
listMain = do
  size1 <- sizeData
  cost1 <- costData
  size2 <- sizeData
  cost2 <- costData
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza

monadPizzaCompare :: Monad m => m Double -> m Double ->
                     m Double -> m Double -> m String
monadPizzaCompare size1Data size2Data cost1Data cost2Data = do
  size1 <- size1Data
  cost1 <- cost1Data
  size2 <- size2Data
  cost2 <- cost2Data
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza
