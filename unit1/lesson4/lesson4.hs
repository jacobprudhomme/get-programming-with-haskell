#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Data.List


ifEvenInc n =
  if even n
  then n + 1
  else n

ifEvenDouble n =
  if even n
  then 2 * n
  else n

ifEvenSquare n =
  if even n
  then n^2
  else n

ifEven myFn x =
  if even x
  then myFn x
  else x

inc n = n + 1

double n = 2 * n

square n = n^2

ifEvenInc' n = ifEven inc n

ifEvenDouble' n = ifEven double n

ifEvenSquare' n = ifEven square n

ifEvenDouble'' n = ifEven (\x -> 2 * x) n

ifEvenCube n = ifEven (\x -> x^3) n

names =
  [("Ian", "Curtis")
  ,("Bernard", "Sumner")
  ,("Peter", "Hook")
  ,("Stephen", "Morris")
  ]

compareLastNames name1 name2 =
  if lastName1 > lastName2
  then GT
  else
    if lastName1 < lastName2
    then LT
    else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2

sortNames = sortBy compareLastNames names

names' =
  [("Ian", "Curtis")
  ,("Abigail", "Curtis")
  ,("Zeno", "Curtis")
  ,("Boris", "Curtis")
  ,("Litty", "Boi")
  ]

compareLastNames' name1 name2 =
  if lastName1 > lastName2
  then GT
  else
    if lastName1 < lastName2
    then LT
    else
      if firstName1 > firstName2
      then GT
      else
        if firstName1 < firstName2
        then LT
        else EQ
  where
    firstName1 = fst name1
    firstName2 = fst name2
    lastName1 = snd name1
    lastName2 = snd name2

addressLetter name location = nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)

sfOffice name =
  if lastName < "L"
  then nameText
    ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText
    ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = (fst name) ++ " " ++ lastName

nyOffice name =
  nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name =
  nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name =
  nameText ++ " - PO Box 101 - Washington, DC, 01010"
  where nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq."

getLocationFunction location =
  case location of
    "dc" -> dcOffice
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter' name location = locationFunction name
  where locationFunction = getLocationFunction location

compareLastNames'' name1 name2 =
  if lastNameComparison == EQ
  then compare firstName1 firstName2
  else lastNameComparison
  where
    firstName1 = fst name1
    firstName2 = fst name2
    lastName1 = snd name1
    lastName2 = snd name2
    lastNameComparison = compare lastName1 lastName2
