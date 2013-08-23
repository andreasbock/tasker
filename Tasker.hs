{-# OPTIONS -Wall #-}
-- module Main(main) where
import Types
import Data.List
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
main = fmap head' getArgs >>= action
  where head' [] = []
        head' (x:_) = x

-- | 'action' returns the appropriate action
-- | based on what it matches.
action :: String -> IO ()
action "-h" = options
action "-l" = listTasks
action "-a" = addTask
action "-d" = deleteTask 1
action "4"  = putStr ""
action _    = options

-- | 'options' prints the user's options.
options :: IO ()
options = mapM_ putStrLn 
          ["You have the following options:",
		   "  -h  display help",
		   "  -l  list current tasks",
		   "  -a  add a task",
		   "  -d  delete a task"]

-- tasksBeforeTime :: DueDate -> [Tasks]

listTasks :: IO ()
listTasks = taskDb >>= BC.readFile >>= putStrLn . BC.unpack

addTask :: IO ()
addTask = Task <$> newId <*> getTime <*> getDesc >>= saveTask

deleteTask :: Int -> IO ()
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

-- | 'newId' returns a new id number based on 
-- | the current one in the ~/tasker/ids file.
newId :: IO Int
newId = do
		  i <- taskIdDb >>= BC.readFile
		  let curId = readByteString i :: Int -- get id, increment and save
 		  taskIdDb >>= flip BC.writeFile (incrementId curId)
		  return curId										
  where incrementId = BC.pack . show . succ

getTime :: IO Day
getTime = do
           putStrLn "Type a due date (empty just adds timestamp):"
           input <- getLine
           if "" == input
           then do
		 		 time <- getCurrentTime
				 return $ utctDay time
           else undefined --parseTime input TODO
-- parseTime :: IO Something
getDesc :: IO B.ByteString
getDesc = putStrLn "Type a brief description of the task:" >> BC.getLine

-- | Helper functions:
readByteString :: Read a => BC.ByteString -> a
readByteString = read . BC.unpack
