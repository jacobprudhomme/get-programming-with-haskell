#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20


echo :: IO ()
echo = getLine >>= putStrLn

main :: IO ()
main = echo

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print $ n * 2

doubleInt :: IO ()
doubleInt = readInt >>= printDouble

echoVerbose :: IO ()
echoVerbose =
  putStrLn "Enter a String and we'll echo it!" >> getLine >>= putStrLn

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

qc :: Num a => a -> IO a
qc = (\n -> return ((+ 2) n))

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f m = m >>= (\x -> return (f x))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf m = mf >>= (\f -> m >>= (\x -> return (f x)))

allApp' :: Monad m => m (a -> b) -> m a -> m b
allApp' mf m = mf >>= (\f -> allFmapM f m)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _  = Nothing
bind (Just x) f = f x
