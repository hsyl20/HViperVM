module ViperVM.Platform.Link (
   Link(..), linkInfo, linkEndpoints, linksBetween,
   linkCapabilities
) where

import qualified ViperVM.Backends.OpenCL.Link as CL
import ViperVM.Platform.Memory
import ViperVM.Platform.LinkCapabilities
import Text.Printf
import Data.Set (Set)

-- | A link between two memories
data Link = CLLink CL.Link
            deriving (Eq,Ord)


instance Show Link where
  show (CLLink l) = show l

-- | Retrieve link information string
linkInfo :: Link -> IO String
linkInfo l@(CLLink {}) = do
   let (e1,e2) = linkEndpoints l
   name1 <- memoryName e1
   name2 <- memoryName e2
   return $ printf "[OpenCL] Link between:\n      - %s\n      - %s" name1 name2

-- | Get memories at each end of a link
linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints (CLLink l) = CL.linkEndpoints l

-- | Get links between a source and a destination from a list of links
linksBetween :: Memory -> Memory -> [Link] -> [Link]
linksBetween m1 m2 = filter ((==) (m1,m2) . linkEndpoints)

linkCapabilities :: Link -> Set LinkCapability
linkCapabilities (CLLink l) = CL.linkCapabilities l
