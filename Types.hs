-- Types for the tasker program
module Types 
	(
	  Task(Task)
	, TaskID
	-- * Exported from Data.Time
    , getCurrentTime
	, toGregorian
	, utctDay
	, Day
	) where

import Data.Time
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

type DueDate = Day
type Description = B.ByteString
type TaskID = Int

data Task = Task TaskID DueDate Description
instance Show Task where
  show (Task id due desc) = show id++" "++show due ++" "++ BC.unpack desc
