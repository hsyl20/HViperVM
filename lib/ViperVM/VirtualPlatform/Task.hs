module ViperVM.VirtualPlatform.Task (
   Task(..), taskExecute
) where

import ViperVM.VirtualPlatform.MetaKernel
import ViperVM.VirtualPlatform.MetaObject
import ViperVM.VirtualPlatform.Object
import ViperVM.Platform.Kernel
import ViperVM.Platform.Proc
import ViperVM.STM.TSet as TSet

import Control.Monad
import Control.Applicative
import Control.Concurrent.STM

-- | Task
data Task = Task {
   metaKernel :: MetaKernel,
   params :: [MetaObject]
}

-- | Synchronous task execution
-- Selected kernel and objects must belong to task meta kernel and meta objects (respectively)
taskExecute :: Task -> Kernel -> Proc -> [Object] -> IO ()
taskExecute task ker proc objs = do
   -- Checks
   let 
      metaKer = metaKernel task
      params' = params task
      validKer = elem ker (kernels metaKer)
      kernelParams = paramsFromObjects metaKer (fmap objectPeer objs)

   unless validKer (error "Invalid kernel")
   unless (canExecute proc ker) (error "Kernel cannot be executed on this processor")

   validObj <- atomically $ do
      forM_ objs lockObject
      and <$> zipWithM TSet.member objs (fmap objects params') 

   unless validObj $ do
      atomically $ forM_ objs unlockObject
      error "Invalid parameter"

   execute proc ker kernelParams
