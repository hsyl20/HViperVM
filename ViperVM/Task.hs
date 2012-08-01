module ViperVM.Task where

import ViperVM.Data
import ViperVM.KernelSet

import Data.Maybe

data TaskParameter = TPReadOnly Data
                   | TPReadWrite Data Data
                   | TPAllocate Data
                   deriving (Eq,Ord)

-- | A task
data Task = Task KernelSet [TaskParameter]
            deriving (Eq,Ord)

instance Show Task where
  show (Task ks _) = show ks

-- | Return output data for the given parameter
paramToData :: TaskParameter -> Data
paramToData (TPReadOnly d) = d
paramToData (TPReadWrite _ d) = d
paramToData (TPAllocate d) = d

-- | Return input data for the given parameter
inputData :: TaskParameter -> Maybe Data
inputData (TPReadOnly d) = Just d
inputData (TPReadWrite d _) = Just d
inputData (TPAllocate _) = Nothing

-- | Return input datas for a list of parameters
inputDatas :: [TaskParameter] -> [Data]
inputDatas ps = catMaybes $ map inputData ps
