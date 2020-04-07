#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

halve :: Int -> Double
halve n = fromIntegral n / 2.0

-- Would have to make a wrapper function like this very often
-- to deal with all the different cases where Maybes can appear
halveMaybe :: Maybe Int -> Maybe Double
halveMaybe Nothing  = Nothing
halveMaybe (Just n) = Just $ halve n
