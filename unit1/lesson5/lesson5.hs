#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

ifEven myFn x =
  if even x
  then myFn x
  else x

inc n = n + 1

genIfEven f = (\x -> ifEven f x)

ifEvenInc = genIfEven inc

genIfXEven x = (\f -> ifEven f x)

getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id ->
  getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://jacobprudhom.me"

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
  hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk311"

genResourceRequestBuilder apiBuilder resource =
  (\id -> apiBuilder resource id)

myOwnExampleUrlBuilder = genResourceRequestBuilder myExampleUrlBuilder "book"

add4 a b c d = a + b + c + d

addXto3 x = (\b c d -> add4 x b c d)

addXYto2 x y = (\c d -> add4 x y c d)

mystery = add4 3

anotherMystery = add4 2 3

exampleUrlBuilder' = getRequestURL "http://jacobprudhom.me"

myExampleUrlBuilder' = exampleUrlBuilder' "1337hAsk311"

myOwnExampleUrlBuilder' = myExampleUrlBuilder' "book"

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
addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

addressLetter' location name = addressLetter name location

flipBinaryArgs f = (\x y -> f y x)

addressLetter'' = flipBinaryArgs addressLetter

addressLetterNY = addressLetter'' "ny"

subtract2 = flip (-) 2

ifEvenInc' = ifEven inc

ifEvenDouble = ifEven (* 2)

ifEvenSquare = ifEven (^ 2)

binaryPartialApplication f x = (\y -> f x y)
