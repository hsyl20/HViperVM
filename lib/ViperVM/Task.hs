module ViperVM.Task where

import ViperVM.Data
import ViperVM.KernelSet

import Data.Maybe

-- | Task data parameters and operation to perform (allocate, duplicate, nop)
data TaskParameter = TPReadOnly Data          -- ^ A data accessed in read-only mode
                   | TPReadWrite Data Data    -- ^ A data that is duplicated into the second data
                   | TPAllocate Data          -- ^ A newly allocated data
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
outputDatas = mapMaybe outputData

-- | Return read only data from a parameter
roData :: TaskParameter -> Maybe Data
roData (TPReadOnly d) = Just d
roData _ = Nothing

-- | Return read only data list
roDatas :: [TaskParameter] -> [Data]
roDatas = mapMaybe roData

-- | Return write-only data from a parameter
woData :: TaskParameter -> Maybe Data
woData (TPAllocate d) = Just d
woData _ = Nothing

-- | Return write-only data list
woDatas :: [TaskParameter] -> [Data]
woDatas = mapMaybe woData

-- | Return read-write input data
rwInputData :: TaskParameter -> Maybe Data
rwInputData (TPReadWrite d _) = Just d
rwInputData _ = Nothing

-- | Return read-write input data list
rwInputDatas :: [TaskParameter] -> [Data]
rwInputDatas = mapMaybe rwInputData

-- | Return read-write output data
rwOutputData :: TaskParameter -> Maybe Data
rwOutputData (TPReadWrite _ d) = Just d
rwOutputData _ = Nothing

-- | Return read-write ouput data list
rwOutputDatas :: [TaskParameter] -> [Data]
rwOutputDatas = mapMaybe rwOutputData
