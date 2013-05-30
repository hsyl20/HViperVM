module ViperVM.Platform.Link (
   Link(..), linkInfo, linkEndpoints, linksBetween
) where

import ViperVM.Backends.OpenCL
import ViperVM.Platform.Memory
import Text.Printf

-- | A link between two memories
data Link = CLLink OpenCLLibrary CLCommandQueue Memory Memory
            deriving (Eq,Ord)

instance Show Link where
  show (CLLink _ _ m1 m2) = printf "OpenCL link between %s and %s" (show m1) (show m2)

-- | Retrieve link information string
linkInfo :: Link -> IO String
linkInfo (CLLink _ _ e1 e2) = do
   name1 <- memName e1
   name2 <- memName e2
   return $ printf "[OpenCL] Link between:\n      - %s\n      - %s" name1 name2

-- | Get memories at each end of a link
linkEndpoints :: Link -> (Memory,Memory)
linkEndpoints (CLLink _ _ m1 m2) = (m1,m2)

-- | Get links between a source and a destination from a list of links
linksBetween :: Memory -> Memory -> [Link] -> [Link]
linksBetween m1 m2 = filter ((==) (m1,m2) . linkEndpoints)
