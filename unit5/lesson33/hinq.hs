#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Control.Applicative
import Control.Monad


data Name = Name
  { firstName :: String
  , lastName  :: String
  }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel
  = Freshman
  | Sophomore
  | Junior
  | Senior
  deriving (Enum, Eq, Ord, Show)

data Student = Student
  { studentId   :: Int
  , gradeLevel  :: GradeLevel
  , studentName :: Name
  } deriving Show

students :: [Student]
students = [ (Student 1 Senior (Name "Audre" "Lorde"))
           , (Student 2 Junior (Name "Leslie" "Silko"))
           , (Student 3 Freshman (Name "Judith" "Butler"))
           , (Student 4 Senior (Name "Guy" "Debord"))
           , (Student 5 Sophomore (Name "Jean" "Beaudrillard"))
           , (Student 6 Junior (Name "Julia" "Kristeva"))
           ]

-- select takes the property (column) you want to select,
-- a list of data (rows) you want to select it from, and
-- returns a list of that property from each piece of data
-- Just with lists: select' :: (a -> b) -> [a] -> [b]
select' :: Monad m => (a -> b) -> m a -> m b
select' prop rows = do
  row <- rows
  return $ prop row

selectExample1 = select' (firstName . studentName) students
selectExample2 = select' gradeLevel students
selectExample3 = select' (\x -> (studentName x, gradeLevel x)) students

-- where takes a predicate and a list of data, and will only
-- return the items in the list for which the predicate is true
-- Just with lists: where' :: (a -> Bool) -> [a] -> [a]
where' :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
where' pred vals = do
  val <- vals
  guard $ pred val
  return val

startsWith :: Char -> String -> Bool
startsWith c str = c == (head str)

whereExample = where' (startsWith 'J' . firstName) (select' studentName students)

data Teacher = Teacher
  { teacherId   :: Int
  , teacherName :: Name
  } deriving Show

teachers :: [Teacher]
teachers = [ (Teacher 100 (Name "Simone" "De Beauvoir"))
           , (Teacher 200 (Name "Susan" "Sontag"))
           ]

data Course = Course
  { courseId    :: Int
  , courseTitle :: String
  , teacher     :: Int
  } deriving Show

courses :: [Course]
courses = [(Course 101 "French" 100), (Course 201 "English" 200)]

-- join takes two lists of data (rows from two tables) and a
-- property for each list, which is what you want to select it,
-- and returns a list of tuples of each row from both lists
-- where some condition between the properties of both is met
-- Just with lists: join' :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]
join' :: (Monad m, Alternative m, Eq c) =>
         m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
join' rows1 rows2 prop1 prop2 = do
  row1 <- rows1
  row2 <- rows2
  let rowPair = (row1, row2)
  guard $ (prop1 (fst rowPair)) == (prop2 (snd rowPair))
  return rowPair

joinExample = join' teachers courses teacherId teacher

-- We want to restructure this to look like select' x join y where z
joinData = join' teachers courses teacherId teacher
whereResult = where' ((== "English") . courseTitle . snd) joinData
selectResult = select' (teacherName . fst) whereResult

hinq' selectQuery joinQuery whereQuery =
  (\joinData ->
    (\whereResult ->
      selectQuery whereResult)
    (whereQuery joinData))
  joinQuery

finalResult :: [Name]
finalResult = hinq' (select' (teacherName . fst))
                    (join' teachers courses teacherId teacher)
                    (where' ((== "English") . courseTitle . snd))

teacherFirstNames :: [String]
teacherFirstNames = hinq' (select' firstName)
                          finalResult
                          (where' (\_ -> True))

data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a) -- Select, join (or data) and where clauses
  | HINQ' (m a -> m b) (m a)             -- Select and join (or data) clauses

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = hinq' sClause jClause wClause
runHINQ (HINQ' sClause jClause)        = hinq' sClause jClause $ where' (const True)

query1 :: HINQ [] (Teacher,Course) Name
query1 = HINQ (select' (teacherName . fst))
              (join' teachers courses teacherId teacher)
              (where' ((== "English") . courseTitle . snd))

query1Result = runHINQ query1

query2 :: HINQ [] Teacher Name
query2 = HINQ' (select' teacherName) teachers

query2Result = runHINQ query2

possibleTeacher :: Maybe Teacher
possibleTeacher = Just $ head teachers

possibleCourse :: Maybe Course
possibleCourse = Just $ head courses

maybeQuery1 :: HINQ Maybe (Teacher,Course) Name
maybeQuery1 = HINQ (select' (teacherName . fst))
                   (join' possibleTeacher possibleCourse teacherId teacher)
                   (where' ((== "French") . courseTitle . snd))

maybeQuery1Example = runHINQ maybeQuery1

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher,Course) Name
maybeQuery2 = HINQ (select' (teacherName . fst))
                   (join' possibleTeacher missingCourse teacherId teacher)
                   (where' ((== "French") . courseTitle . snd))

maybeQuery2Example = runHINQ maybeQuery2

data Enrollment = Enrollment
  { student :: Int
  , course  :: Int
  } deriving Show

enrollments :: [Enrollment]
enrollments = [ (Enrollment 1 101)
              , (Enrollment 2 101)
              , (Enrollment 2 201)
              , (Enrollment 3 101)
              , (Enrollment 4 201)
              , (Enrollment 4 101)
              , (Enrollment 5 101)
              , (Enrollment 6 201)
              ]

-- GHCi says: studentEnrollmentsQ :: HINQ [] (Student,Enrollment) (Name,Int)
studentEnrollmentsQ =
  HINQ' (select' (\(std, enr) -> (studentName std, course enr)))
        (join' students enrollments studentId student)

studentEnrollments :: [(Name,Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ = HINQ (select' (fst . fst))
                        (join' studentEnrollments courses snd courseId)
                        (where' ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where courseQuery = HINQ (select' (fst . fst))
                           (join' studentEnrollments courses snd courseId)
                           (where' ((== courseName) . courseTitle . snd))

englishStudents' = getEnrollments "English"
frenchStudents = getEnrollments "French"
