#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

data FourLetterAlph = L1 | L2 | L3 | L4 deriving (Bounded, Enum, Show)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphSize c = toEnum rotatedC
  where
    halfAlphSize = alphSize `div` 2
    offset = fromEnum c + halfAlphSize
    rotatedC = offset `mod` alphSize

rotChar :: Char -> Char
rotChar charToEncrypt = rotN alphSize charToEncrypt
  where
    largestCharNum = fromEnum (maxBound :: Char)
    alphSize = largestCharNum + 1

message :: [FourLetterAlph]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphEncoder :: [FourLetterAlph] -> [FourLetterAlph]
fourLetterAlphEncoder msg = map rot4L msg
  where
    alphSize = fromEnum (maxBound :: FourLetterAlph) + 1
    rot4L = rotN alphSize

data ThreeLetterAlph
  = Alpha
  | Beta
  | Gamma
  deriving (Bounded, Enum, Show)

message' :: [ThreeLetterAlph]
message' = [Alpha, Alpha, Beta, Alpha, Gamma]

threeLetterAlphEncoder :: [ThreeLetterAlph] -> [ThreeLetterAlph]
threeLetterAlphEncoder msg = map rot3L msg
  where
    alphSize = fromEnum (maxBound :: ThreeLetterAlph) + 1
    rot3L = rotN alphSize

rotNDecode :: (Bounded a, Enum a) => Int -> a -> a
rotNDecode alphSize c = toEnum rotatedC
  where
    halfAlphSize = alphSize `div` 2
    offset = fromEnum c + halfAlphSize + (if even alphSize then 0 else 1)
    rotatedC = offset `mod` alphSize

threeLetterAlphDecoder :: [ThreeLetterAlph] -> [ThreeLetterAlph]
threeLetterAlphDecoder msg = map rot3L msg
  where
    alphSize = fromEnum (maxBound :: ThreeLetterAlph) + 1
    rot3L = rotNDecode alphSize

rotEncoder :: String -> String
rotEncoder msg = map rotChar msg

rotDecoder :: String -> String
rotDecoder msg = map rotCharDecode msg
  where
    alphSize = fromEnum (maxBound :: Char) + 1
    rotCharDecode = rotNDecode alphSize

fourLetterAlphDecoder :: [FourLetterAlph] -> [FourLetterAlph]
fourLetterAlphDecoder msg = map rot4L msg
  where
    alphSize = fromEnum (maxBound :: FourLetterAlph) + 1
    rot4L = rotNDecode alphSize

xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && not (val1 && val2)

xorPair :: (Bool,Bool) -> Bool
xorPair (val1, val2) = xorBool val1 val2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair $ zip list1 list2

type Bits = [Bool]

intToBitsRev :: Int -> Bits
intToBitsRev 0 = [False]
intToBitsRev 1 = [True]
intToBitsRev n =
  if lsb == 0
  then False : intToBitsRev nextNum
  else True : intToBitsRev nextNum
    where
      lsb = n `mod` 2
      nextNum = n `div` 2

maxNumBits :: Int
maxNumBits = length $ intToBitsRev maxBound

intToBits :: Int -> Bits
intToBits n = padding ++ bits
  where
    bits = reverse $ intToBitsRev n
    numPaddingBits = maxNumBits - (length bits)
    padding = take numPaddingBits $ cycle [False]

charToBits :: Char -> Bits
charToBits c = intToBits $ fromEnum c

bitsToInt :: Bits -> Int
bitsToInt bits = sum $ map (\(idx, _) -> 2^idx) only1s
  where
    size = length bits
    indices = [size - 1, size - 2 .. 0]
    only1s = filter (\(_, bit) -> bit) (zip indices bits)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum $ bitsToInt bits

myPad :: String
myPad = "Shhhhhh"

myPlaintext :: String
myPlaintext  = "Haskell"

applyOTP :: String -> String -> [Bits]
applyOTP pad plaintext =
  map (\(padBit, plaintextBit) ->
    padBit `xor` plaintextBit)
    $ zip padBits plaintextBits
  where
    padBits = map charToBits pad
    plaintextBits = map charToBits plaintext

oneTimePad :: String -> String -> String
oneTimePad pad plaintext = map bitsToChar ciphertext
  where ciphertext = applyOTP pad plaintext

encodeDecodeOTP :: String -> String
encodeDecodeOTP = oneTimePad myPad

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot msg = rotEncoder msg
  decode Rot msg = rotDecoder msg

data OTP = OTP String

instance Cipher OTP where
  encode (OTP pad) msg = oneTimePad pad msg
  decode = encode

myOTP :: OTP
myOTP = OTP $ cycle [minBound .. maxBound]

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNum seed = (a * seed + b) `mod` maxNum

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

data StreamCipher = StreamCipher (Int -> Int) Int

prngStream :: (Int -> Int) -> Int -> [Int]
prngStream prng seed = nextSeed : prngStream prng nextSeed
  where nextSeed = prng seed

instance Cipher StreamCipher where
  encode (StreamCipher prng seed) msg = oneTimePad pad msg
    where
      stream = prngStream prng seed
      pad = map toEnum stream
  decode = encode
