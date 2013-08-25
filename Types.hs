-- Types for the tasker program
module Types 
	(
	  Task(Task)
	, TaskID
-- 	, DbEntry
	-- * Exported from Data.Time
    , getCurrentTime
	, toGregorian
	, utctDay
	, DueDate
	) where

import Data.Time
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

type DueDate = Day
type Description = B.ByteString
type TaskID = Int

data Task = Task Description DueDate TaskID 
instance Show Task where
  show (Task desc due id) = BC.unpack desc++" "++show due++" "++show id
