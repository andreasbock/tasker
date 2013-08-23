{-# OPTIONS -Wall #-}
-- module Main(main) where
import Types -- TODO: organise imports
import Data.List
import System.IO
import System.Posix.User
import System.Directory
import System.Environment
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

homeDir, taskDir, taskDb, taskIdDb :: IO String
homeDir = ("/home/"++) <$> getLoginName
taskDir = dir >>= createDirectoryIfMissing False >> dir
  where dir = (++) <$> homeDir <*> pure "/.tasker/"
taskDb   = (++) <$> taskDir <*> pure "tasks"
taskIdDb = (++) <$> taskDir <*> pure "id"

-- | The 'main' function starts.
main :: IO ()
main = getArgs >>= action

-- | 'action' returns the appropriate action
-- | based on what it matches.
action :: [String] -> IO () -- TODO: rewrite this, getopt for Haskell?
action ("-l":_)   = listTasks
action ("-a":_)   = addTask
action ("-d":x:_) = deleteTask (read x :: TaskID)
action _    	  = options

-- | 'options' prints the user's options.
options :: IO ()
options = mapM_ putStrLn 
          ["You have the following options:",
		   "  -l  list current tasks",
		   "  -a  add a task",
		   "  -d  delete a task"]

-- tasksBeforeTime :: DueDate -> [Tasks]

listTasks :: IO ()
listTasks = taskDb >>= BC.readFile >>= putStrLn . BC.unpack

addTask :: IO ()
addTask = Task <$> newId <*> getTime <*> getDesc >>= saveTask

-- | 'deleteTask' takes a TaskID and deletes
-- | the desired entry from the database.
deleteTask :: TaskID -> IO ()
deleteTask i = do
			    db <- fmap (toWords . BC.lines) $ taskDb >>= BC.readFile
			    let filtered = filter (\x -> i /= (readByteString . head) x) db
			    let out = map BC.unwords filtered -- concat into list of tasks 
			    writeDB out
  where toWords = fmap BC.words

-- | 'writeDB' takes a list of bytestrings and
-- | writes them to the database.
writeDB :: [BC.ByteString] -> IO ()
writeDB []  = return ()
writeDB out = writeDB' $ BC.concat $ intersperse newLine out ++ [newLine]
  where writeDB' b = taskDb >>= flip BC.writeFile b
        newLine = BC.pack "\n"

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
getTime :: IO Day
getTime = do
           putStr "Enter desired due date \"yyyy-mm-dd\" (empty for timestamp): "
           hFlush stdout
           i <- getLine 
           if "" == i
           then getTimestamp
           else do
				 case reads i of
				   [(day,"")] -> return day
				   _		  -> putStr "Erroneous input!" >> getTime

-- | 'getTimestamp' returns the current date.
getTimestamp :: IO Day				 
getTimestamp = getCurrentTime >>= return . utctDay

-- | 'getDesc' is a simple wrapper for BC.getLine.
getDesc :: IO B.ByteString
getDesc = putStrLn "Type a brief description of the task:" >> BC.getLine

-- | Helper functions:
readByteString :: Read a => BC.ByteString -> a
readByteString = read . BC.unpack
