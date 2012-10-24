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

-- | Return output data for the given parameter
outputData :: TaskParameter -> Maybe Data
outputData (TPReadOnly _) = Nothing
outputData (TPReadWrite _ d) = Just d
outputData (TPAllocate d) = Just d

-- | Return output datas for a list of parameters
outputDatas :: [TaskParameter] -> [Data]
outputDatas ps = catMaybes $ map outputData ps

-- | Return read only data from a parameter
roData :: TaskParameter -> Maybe Data
roData (TPReadOnly d) = Just d
roData _ = Nothing

-- | Return read only data list
roDatas :: [TaskParameter] -> [Data]
roDatas ps = catMaybes $ map roData ps

-- | Return write-only data from a parameter
woData :: TaskParameter -> Maybe Data
woData (TPAllocate d) = Just d
woData _ = Nothing

-- | Return write-only data list
woDatas :: [TaskParameter] -> [Data]
woDatas ps = catMaybes $ map woData ps

-- | Return read-write input data
rwInputData :: TaskParameter -> Maybe Data
rwInputData (TPReadWrite d _) = Just d
rwInputData _ = Nothing

-- | Return read-write input data list
rwInputDatas :: [TaskParameter] -> [Data]
rwInputDatas ps = catMaybes $ map rwInputData ps

-- | Return read-write output data
rwOutputData :: TaskParameter -> Maybe Data
rwOutputData (TPReadWrite _ d) = Just d
rwOutputData _ = Nothing

-- | Return read-write ouput data list
rwOutputDatas :: [TaskParameter] -> [Data]
rwOutputDatas ps = catMaybes $ map rwOutputData ps
