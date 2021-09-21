{-# LANGUAGE OverloadedStrings #-}

module Databases where

import           Data.Time                      ( Day
                                                , UTCTime(utctDay)
                                                , getCurrentTime
                                                )
import           Database.SQLite.Simple         ( Connection
                                                , FromRow(..)
                                                , Only(Only)
                                                , Query
                                                , close
                                                , execute
                                                , field
                                                , open
                                                , query
                                                , query_
                                                )

data Tool = Tool
    { toolId        :: Int
    , name          :: String
    , description   :: String
    , lastReturned  :: Day
    , timesBorrowed :: Int
    }

data User = User
    { userId   :: Int
    , userName :: String
    }

instance Show User where
    show user = mconcat [show $ userId user, ".) ", userName user]

instance Show Tool where
    show tool = mconcat
        [ show $ toolId tool
        , ".) "
        , name tool
        , "\n description: "
        , description tool
        , "\n last time returned: "
        , show $ lastReturned tool
        , "\n times borrowed: "
        , show $ timesBorrowed tool
        , "\n"
        ]

instance FromRow User where
    fromRow = User <$> field <*> field

instance FromRow Tool where
    fromRow = Tool <$> field <*> field <*> field <*> field <*> field

dbFileName :: String
dbFileName = "tools.db"

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

addUser :: String -> IO ()
addUser userName = withConn dbFileName $ \conn -> do
    execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
    putStrLn "User was added successfully"

addTool :: String -> String -> Day -> IO ()
addTool toolName toolDesc day = withConn "tools.db" $ \conn -> do
    execute conn
            "INSERT INTO tools (name,description,lastReturned,timesBorrowed) VALUES (?,?,?,?)"
            (toolName, toolDesc, day, 0 :: Int)
    putStrLn "Tool was added successfully"

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn dbFileName
    $ \conn -> execute conn "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)" (userId, toolId)

printUsers :: IO ()
printUsers = withConn dbFileName $ \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn dbFileName $ \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable =
    printToolQuery $ mconcat ["SELECT * FROM tools ", "WHERE id NOT IN ", "(SELECT tool_id FROM checkedout);"]

printCheckedout :: IO ()
printCheckedout =
    printToolQuery $ mconcat ["SELECT * FROM tools ", "WHERE id IN ", "(SELECT tool_id FROM checkedout);"]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing []      = Nothing
firstOrNothing (x : _) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool { lastReturned = date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing     = putStrLn "ID does not exist"
updateOrWarn (Just tool) = withConn dbFileName $ \conn -> do
    let q = mconcat ["UPDATE TOOLS SET ", "lastReturned = ?,", " timesBorrowed = ? ", "WHERE ID = ?;"]
    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
    putStrLn "Tool data was updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn dbFileName $ \conn -> do
    tool       <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId =
    withConn "tools.db" $ \conn -> execute conn "DELETE FROM checkedout WHERE tool_id = (?);" (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    putStrLn "New username:"
    userName <- getLine
    addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
    putStrLn "Enter user ID:"
    userId <- read <$> getLine
    putStrLn "Enter tool ID:"
    toolId <- read <$> getLine
    checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
    putStrLn "Enter tool ID:"
    toolId <- read <$> getLine
    checkinAndUpdate toolId

promptAndAddTool :: IO ()
promptAndAddTool = do
    putStrLn "Enter tool name:"
    toolName <- getLine
    putStrLn "Enter tool description:"
    toolDesc   <- getLine
    currentDay <- utctDay <$> getCurrentTime
    addTool toolName toolDesc currentDay

performCommand :: String -> IO ()
performCommand "users"    = printUsers >> main
performCommand "tools"    = printTools >> main
performCommand "adduser"  = promptAndAddUser >> main
performCommand "addtool"  = promptAndAddTool >> main
performCommand "checkout" = promptAndCheckout >> main
performCommand "checkin"  = promptAndCheckin >> main
performCommand "in"       = printAvailable >> main
performCommand "out"      = printCheckedout >> main
performCommand "quit"     = putStrLn "Bye!"
performCommand _          = putStrLn "Command not found" >> main

main :: IO ()
main = do
    command <- getLine
    performCommand command
