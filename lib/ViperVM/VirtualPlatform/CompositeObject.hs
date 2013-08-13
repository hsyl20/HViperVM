module ViperVM.VirtualPlatform.CompositeObject where

import ViperVM.Runtime.Memory.Descriptor
import ViperVM.Runtime.Memory.MetaObject

-- | A object that can be rebuilt from other objects
data CompositeObject = CompositeObject {
   descriptor :: Descriptor, -- ^ Descriptor of the composite data
   parts :: [(Object -> Object, MetaObject)] -- ^ Filter to apply and source data
}


fusion :: CompositeObject -> IO MetaObject
fusion obj = 
   

-- | Unsplit a list of lists of matrices
unsplitMatrix :: MetaMemory -> MetaMatrix -> IO MetaObject
unsplitMatrix mem os = do
   -- Compute output matrix dimensions
   let dos = fmap (fmap descriptor) os
       (row1,col1) = (head dos, fmap head dos)
       w = matrixDescWidth (head row1)
       h = matrixDescHeight (head row1)
       gw = sum (fmap matrixDescWidth row1)
       gh = sum (fmap matrixDescHeight col1)
       p = matrixDescCellType (head row1)
       desc = MatrixDesc p gw gh
       om = objectManager mem

   -- Allocate output shared object
   so <- atomically (allocate mem desc)

   -- Allocate instance in host memory (FIXME we could improve this)
   let hostMem = head . hostMemories $ virtualMemoryPlatform mem
   MatrixObject m <- allocateInstance mem so hostMem
   let ms = matrixSplit m w h

   let srcDst = zip (concat os) (concat ms)

   -- Perform transfers
   -- FIXME: transfers are performed sequentially
   forM_ srcDst $ \(src,dst) -> do
      Set.elems <$> atomically (allInstances src) >>= \case
         [] -> error "Uninitialized object accessed in read-only mode"
         srcInstance:_ -> do
            transferObject om srcInstance (MatrixObject dst)
   
   -- Attach and return instance
   atomically (attachInstance so (MatrixObject m))
   return so

