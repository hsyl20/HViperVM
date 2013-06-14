module ViperVM.Platform.Link (
   Link(..), linkInfo, linkEndpoints, linksBetween,
   linkCapabilities, LinkCapability(..)
) where

import ViperVM.Backends.OpenCL.Types
import ViperVM.Backends.OpenCL.Loader
import ViperVM.Platform.Memory
import Data.Set (Set)
import Text.Printf

-- | A link between two memories
data Link = CLLink OpenCLLibrary CLCommandQueue Memory Memory (Set LinkCapability)
            deriving (Eq,Ord)

data LinkCapability = Transfer2D
                      deriving (Eq, Ord)

instance Show Link where
  show (CLLink _ _ m1 m2 _) = printf "OpenCL link between %s and %s" (show m1) (show m2)

-- | Retrieve link information string
linkInfo :: Link -> IO String
linkInfo l@(CLLink {}) = do
   let (e1,e2) = linkEndpoints l
   name1 <- memName e1
   name2 <- memName e2
   return $ printf "[OpenCL] Link between:\n      - %s\n      - %s" name1 name2

-- | Get memories at each end of a link
linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints (CLLink _ _ m1 m2 _) = (m1,m2)

-- | Get links between a source and a destination from a list of links
linksBetween :: Memory -> Memory -> [Link] -> [Link]
linksBetween m1 m2 = filter ((==) (m1,m2) . linkEndpoints)

-- | Check if the link supports a given capability
linkCapabilities :: Link -> Set LinkCapability
linkCapabilities (CLLink _ _ _ _ cap) = cap
