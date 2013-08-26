{-# OPTIONS -Wall #-}
module Main(main) where

import Types
import System.IO
import Data.List
import Data.List.Split
import System.Directory
import System.Posix.User
import System.Environment
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

homeDir, taskDir, taskDb, taskIdDb :: IO String
homeDir = ("/home/"++) <$> getLoginName
taskDir = dir >>= createDirectoryIfMissing False >> dir
  where dir = (++) <$> homeDir <*> pure "/.tasker/"
taskDb   = (++) <$> taskDir <*> pure "tasks"
taskIdDb = (++) <$> taskDir <*> pure "id"

-- | The 'main' function.
main :: IO ()
main = getArgs >>= action

-- | 'action' returns the appropriate action
-- | based on what it matches.
action :: [String] -> IO () -- TODO: rewrite this, getopt for Haskell?
action ("-a":_)   = addTask
action ("-d":x:_) = deleteTask (read x :: TaskID)
action ("-h":_)   = options
action _    	  = listTasks

-- | 'options' prints the user's options.
options :: IO ()
options = mapM_ putStrLn 
          ["Usage: tasker [ OPTIONS ]",
		   "where  OPTIONS := { -l[ist]",
		   "                    -a[dd]",
		   "                    -d[elete]}"]

-- tasksBeforeTime :: DueDate -> [Task]

-- | 'listTasks' prints the database to stdout.
listTasks :: IO ()
listTasks = do
			 tasks <- taskDb >>= BC.readFile
			 let pretty = BC.intercalate formatting $ BC.lines tasks
			 BC.putStr $ BC.pack "- " -- intercalate doesn't prepend
			 BC.putStr pretty
  where formatting = BC.pack "\n- "

-- | 'addTask' requests time and description from
-- | stdin to append a new task to the database.
addTask :: IO ()
addTask = Task <$> getDesc <*> getTime <*> newId >>= saveTask

-- | 'deleteTask' takes a TaskID and deletes
-- | the desired entry from the database.
deleteTask :: TaskID -> IO ()
deleteTask i = dbOperation (\entry -> (BC.pack . show) i /= last entry)

-- | 'writeDB' takes a list of bytestrings and
-- | writes them to the database.
writeDB :: [BC.ByteString] -> IO ()
writeDB []  = return ()
writeDB out = writeDB' $ BC.concat $ intersperse newLine out ++ [newLine]
  where writeDB' b = taskDb >>= flip BC.writeFile b
        newLine = BC.pack "\n"

-- | 'dbOperation' takes a function and
-- | filters the database based on the predicate.
dbOperation :: ([BC.ByteString] -> Bool) -> IO ()
dbOperation foo = do
					db <- fmap (toWords . BC.lines) $ taskDb >>= BC.readFile
					let filtered = filter foo db
					let out = map BC.unwords filtered -- concat into list of tasks 
					writeDB out
  where toWords = fmap BC.words

-- | 'saveTask' appends a task to the database file.
saveTask :: Task -> IO ()
saveTask task = do 
				 i <- taskDb
				 BC.appendFile i $ BC.pack $ show task ++ "\n"

-- | 'newId' returns a new TaskId number based on 
-- | the current one in the ~/tasker/ids file.
newId :: IO TaskID
newId = do
		  i <- taskIdDb >>= BC.readFile
		  let curId = readByteString i :: Int -- get id, increment and save
 		  taskIdDb >>= flip BC.writeFile (incrementId curId)
		  return curId										
  where incrementId = BC.pack . show . succ

-- | 'getTime' asks the user to supply a due date
-- | or simply applies a timestamp to the task.
getTime :: IO DueDate
getTime = do
           putStr "Enter desired due date \"dd mm yyyy\" (empty for timestamp): "
           hFlush stdout
           i <- fmap (splitOn " ") getLine
           if [""] == i
           then getTimestamp
           else do
                 let date = intercalate "-" $ reverse i
                 case reads date of
				   [(day,"")] -> return day
				   _		  -> putStr "Erroneous input!" >> getTime

-- | 'getTimestamp' returns the current date.
getTimestamp :: IO DueDate				 
getTimestamp = liftM utctDay getCurrentTime

-- | 'getDesc' is a simple wrapper for BC.getLine.
getDesc :: IO B.ByteString
getDesc = putStrLn "Type a brief description of the task:" >> BC.getLine

-- | Helper functions:
readByteString :: Read a => BC.ByteString -> a
readByteString = read . BC.unpack
