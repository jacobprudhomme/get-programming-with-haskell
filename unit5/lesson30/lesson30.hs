#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import qualified Data.Map as Map


functorExample = (+ 2) <$> Just 3

applicativeExample = pure (+) <*> Just 3 <*> Just 2

monadExample :: IO ()
monadExample = do
  putStrLn "Remember do-notation!"
  putStrLn "It makes things easy."

type Username = String
type GamerId = Int
type PlayerCredits = Int

usernameDB :: Map.Map GamerId Username
usernameDB = Map.fromList
  [ (1, "xX420DragonSlayer69Xx")
  , (2, "KINGinYELLOW")
  , (3, "itsjimfromwork")
  , (4, "`baktix`")
  , (5, "the_thoth")
  , (6, "dec_mcp")
  ]

creditsDB :: Map.Map Username PlayerCredits
creditsDB = Map.fromList
  [ ("xX420DragonSlayer69Xx", 2000)
  , ("KINGinYELLOW", 15000)
  , ("itsjimfromwork", 300)
  , ("`baktix`", 12)
  , ("the_thoth", 50000)
  , ("dec_mcp", 150000)
  ]

lookupUsername :: GamerId -> Maybe Username
lookupUsername id = Map.lookup id usernameDB

lookupCredits :: Username -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

{- Tentative type of missing linking function:
   Maybe Username -> (Username -> Maybe PlayerCredits) -> Maybe PlayerCredits
   Or thinking with the tools we have so far: Applicative f => f a -> (a -> f b) -> f b
   Using Applicative because it's more powerful than Functor; if we can't solve
   the problem with it, then we can't solve it with Functor either.
-}

altLookupCredits :: Maybe Username -> Maybe PlayerCredits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits $ lookupUsername id

creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = pure lookupCredits <*> lookupUsername id

{- echo :: IO (); gets line of input and prints it right back out
   getLine :: IO String
   putStrLn :: String -> IO ()
   Need a function IO String -> (String -> IO ()) -> IO () to do this
   Generalizing it: f a -> (a -> f b) -> f b
   Side note: we cannot just build a function similar to altLookupCredits
   to unwrap the Username from IO, because we cannot pattern match on IO!
-}

creditsFromId' :: GamerId -> Maybe PlayerCredits
creditsFromId' id = lookupUsername id >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList
  [ (1001, 1)
  , (1002, 2)
  , (1003, 3)
  , (1004, 4)
  , (1005, 5)
  , (1006, 6)
  ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWillCoId :: WillCoId -> Maybe PlayerCredits
creditsFromWillCoId id = lookupGamerId id >>= lookupUsername >>= lookupCredits
