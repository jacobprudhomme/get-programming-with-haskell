module Main where

import Control.Applicative
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import qualified Data.Text as T


data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Day
  , timesBorrowed :: Int
  }

data User = User { userId :: Int, username :: String }

instance Show Tool where
  show tool = mconcat [ show $ toolId tool
                      , ".) "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n"
                      ]

instance Show User where
  show user = mconcat [ show $ userId user
                      , ".) "
                      , username user
                      ]

addUser' :: String -> IO ()
addUser' username = do
  conn <- open "tools.db"
  execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
  putStrLn "User added"
  close conn

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

addUser :: String -> IO ()
addUser username = withConn "tools.db" $
  \conn -> do
    execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
    putStrLn "User added"

addTool :: String -> String -> IO ()
addTool toolName toolDesc = withConn "tools.db" $
  \conn -> do
    currentDay <- utctDay <$> getCurrentTime
    execute conn
            "INSERT INTO tools (name, description, lastReturned, timesBorrowed) VALUES (?, ?, ?, ?)"
            (toolName, toolDesc, currentDay, 0 :: Int)
    putStrLn "Tool added"

checkoutTool :: Int -> Int -> IO ()
checkoutTool userId toolId = withConn "tools.db" $
  \conn -> execute conn
    "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
    (userId, toolId)

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance FromRow User where
  fromRow = User <$> field
                 <*> field

printAllUsers :: IO ()
printAllUsers = withConn "tools.db" $
  \conn -> do
    res <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print res

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
  \conn -> do
    res <- query_ conn q :: IO [Tool]
    mapM_ print res

printAllTools :: IO ()
printAllTools = printToolQuery "SELECT * FROM tools;"

printAvailableTools :: IO ()
printAvailableTools = printToolQuery $ mconcat [ "SELECT * FROM tools "
                                               , "WHERE id NOT IN "
                                               , "(SELECT tool_id FROM checkedout);"
                                               ]

printCheckedoutTools :: IO ()
printCheckedoutTools = printToolQuery $ mconcat [ "SELECT * FROM tools "
                                                , "WHERE id IN "
                                                , "(SELECT tool_id FROM checkedout);"
                                                ]

firstOrNothing :: [a] -> Maybe a
firstOrNothing []    = Nothing
firstOrNothing (x:_) = Just x

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  res <- query conn
               "SELECT * FROM tools WHERE id = (?)"
               (Only toolId)
               :: IO [Tool]
  return $ firstOrNothing res

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
  { lastReturned = date
  , timesBorrowed = timesBorrowed tool + 1
  }

updateToolOrWarn :: Maybe Tool -> IO ()
updateToolOrWarn Nothing     = putStrLn "ID not found"
updateToolOrWarn (Just tool) = withConn "tools.db" $
  \conn -> do
    let q = mconcat [ "UPDATE TOOLS SET "
                    , "lastReturned = ?, "
                    , "timesBorrowed = ? "
                    , "WHERE id = ?;"
                    ]
    execute conn q ( lastReturned tool
                   , timesBorrowed tool
                   , toolId tool
                   )
    putStrLn "Tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
  \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool
                                 <*> pure currentDay
    updateToolOrWarn updatedTool

-- This can also be done, but is a little less useful
instance ToRow Tool where
  toRow tool = [ SQLInteger $ fromIntegral $ toolId tool
               , SQLText $ T.pack $ name tool
               , SQLText $ T.pack $ description tool
               , SQLText $ T.pack $ show $ lastReturned tool
               , SQLInteger $ fromIntegral $ timesBorrowed tool
               ]

checkinTool :: Int -> IO ()
checkinTool toolId = withConn "tools.db" $
  \conn -> execute conn
                   "DELETE FROM checkedout WHERE tool_id = (?);"
                   (Only toolId)

checkinAndUpdateTool :: Int -> IO ()
checkinAndUpdateTool toolId = do
  checkinTool toolId
  updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "Enter new username:"
  username <- getLine
  addUser username

promptAndAddTool :: IO ()
promptAndAddTool = do
  putStrLn "Enter the name of the tool:"
  toolName <- getLine
  putStrLn "Enter the description for the tool:"
  toolDesc <- getLine
  addTool toolName toolDesc

promptAndCheckoutTool :: IO ()
promptAndCheckoutTool = do
  putStrLn "Enter the ID of the user:"
  userId <- read <$> getLine
  putStrLn "Enter the ID of the tool:"
  toolId <- read <$> getLine
  checkoutTool userId toolId

promptAndCheckinTool :: IO ()
promptAndCheckinTool = do
  putStrLn "Enter the ID of the tool:"
  toolId <- read <$> getLine
  checkinAndUpdateTool toolId

performCommand :: String -> IO ()
performCommand cmd
  | cmd == "users"    = printAllUsers >> main
  | cmd == "tools"    = printAllTools >> main
  | cmd == "adduser"  = promptAndAddUser >> main
  | cmd == "addtool"  = promptAndAddTool >> main
  | cmd == "checkout" = promptAndCheckoutTool >> main
  | cmd == "checkin"  = promptAndCheckinTool >> main
  | cmd == "in"       = printAvailableTools >> main
  | cmd == "out"      = printCheckedoutTools >> main
  | cmd == "quit"     = putStrLn "Bye!"
  | otherwise         = putStrLn "That is not a command" >> main

main :: IO ()
main = do
  putStrLn "Enter a command:"
  cmd <- getLine
  performCommand cmd
