#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

type FirstName = String
type LastName  = String
type Age       = Int
type Height    = Int

-- patientInfo :: String -> String -> Int -> Int -> String
patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height =
  name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (String,String)

firstName :: PatientName -> FirstName
firstName patient = fst patient
lastName :: PatientName -> LastName
lastName patient = snd patient

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' (fname,lname) age height =
  name ++ " " ++ ageHeight
  where
    name = fname ++ ", " ++ lname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

data Bool' = True' | False'

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O deriving (Eq)
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _  = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeSmith :: Patient
janeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 34 65 130 (BloodType O Neg)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

data Patient' = Patient'
  { name :: Name
  , sex :: Sex
  , age :: Int
  , height :: Int
  , weight :: Int
  , bloodType :: BloodType
  }

jackieSmith :: Patient'
jackieSmith = Patient'
  { name = Name "Jackie" "Smith"
  , age = 43
  , sex = Female
  , height = 62
  , weight = 115
  , bloodType = BloodType O Neg
  }

heightJS = height jackieSmith

bloodTypeJS = showBloodType $ bloodType jackieSmith

nameJS = showName $ name jackieSmith

jackieSmithUpdated = jackieSmith { age = 44 }

canDonateTo' :: Patient' -> Patient' -> Bool
canDonateTo' Patient'{bloodType=bt1} Patient'{bloodType=bt2}
  | abo1 == O    = True
  | abo2 == AB   = True
  | abo1 == abo2 = True
  | otherwise    = False
  where
    (BloodType abo1 _) = bt1
    (BloodType abo2 _) = bt2

showName' :: Patient' -> String
showName' Patient'{name=(Name fname lname)} =
  "Patient Name: " ++ lname ++ ", " ++ fname

showSex :: Patient' -> String
showSex Patient'{sex=Male}   = "Sex: Male"
showSex Patient'{sex=Female} = "Sex: Female"

showAge :: Patient' -> String
showAge Patient'{age=a} = "Age: " ++ show a

showHeight :: Patient' -> String
showHeight Patient'{height=h} = "Height: " ++ show h ++ "in."

showWeight :: Patient' -> String
showWeight Patient'{weight=w} = "Weight: " ++ show w ++ "lbs."

showBloodType' :: Patient' -> String
showBloodType' Patient'{bloodType=bt} = "Blood Type: " ++ showBloodType bt

patientSummary :: Patient' -> String
patientSummary patient =
  "***************\n" ++
  showName' patient ++ "\n" ++
  showSex patient ++ "\n" ++
  showAge patient ++ "\n" ++
  showHeight patient ++ "\n" ++
  showWeight patient ++ "\n" ++
  showBloodType' patient ++ "\n" ++
  "***************\n"
