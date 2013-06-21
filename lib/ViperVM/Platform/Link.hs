module ViperVM.Platform.Link (
   Link(..), linkInfo, linkEndpoints, linksBetween,
   linkCapabilities
) where

import qualified ViperVM.Backends.OpenCL.Link as CL
import qualified ViperVM.Backends.Host.Link as Host
import ViperVM.Platform.Memory
import ViperVM.Platform.LinkCapabilities
import Text.Printf
import Data.Set (Set)
import Control.Arrow

-- | A link between two memories
data Link = 
     CLLink CL.Link
   | HostLink Host.Link
     deriving (Eq,Ord)


instance Show Link where
  show (CLLink l) = show l
  show (HostLink l) = show l


-- | Retrieve link information string
linkInfo :: Link -> String
linkInfo l = if linkSource l == linkTarget l 
      then printf "Loopback link in %s" name1
      else printf "Link between %s and %s" name1 name2
   where
      name1 = memoryName (linkSource l)
      name2 = memoryName (linkTarget l)
      
-- | Get memories at each end of a link
linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints = linkSource &&& linkTarget

-- | Get link source memory
linkSource :: Link -> Memory
linkSource (CLLink l) = CL.linkSource l
linkSource (HostLink l) = Host.linkSource l

-- | Get link target memory
linkTarget :: Link -> Memory
linkTarget (CLLink l) = CL.linkTarget l
linkTarget (HostLink l) = Host.linkTarget l

-- | Get links between a source and a destination from a list of links
linksBetween :: Memory -> Memory -> [Link] -> [Link]
linksBetween m1 m2 = filter ((==) (m1,m2) . linkEndpoints)

-- | Retrieve link capabilities
linkCapabilities :: Link -> Set LinkCapability
linkCapabilities (CLLink l) = CL.linkCapabilities l
linkCapabilities (HostLink l) = Host.linkCapabilities l
