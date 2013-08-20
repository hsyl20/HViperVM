{-# LANGUAGE LambdaCase #-}
module ViperVM.VirtualPlatform.FunctionalKernel where

import ViperVM.VirtualPlatform.MetaKernel (MetaKernel)
import ViperVM.VirtualPlatform.Descriptor
import ViperVM.VirtualPlatform.MetaObject

import ViperVM.Graph.Graph
import ViperVM.Graph.Builtins

data Prototype = Prototype {
   inputs :: [ObjectType],
   output :: ObjectType
}

data FunctionalKernel = FunctionalKernel {
   proto :: Prototype,
   metaKernelParams :: [MetaObject] -> IO [MetaObject],
   metaKernelResult :: [MetaObject] -> MetaObject,
   metaKernel :: MetaKernel
}

type MakeBuiltin = (Expr -> MetaObject) -> (MetaObject -> Expr) -> (MetaKernel -> [MetaObject] -> IO ()) -> IO Builtin

makeBuiltin :: FunctionalKernel -> MakeBuiltin
makeBuiltin ok readData writeData exec = do
   let toEval = fmap (const True) (inputs (proto ok))
   return $ Builtin toEval $ \case
      (args,_) -> do
         let args' = fmap readData args
         kerArgs <- metaKernelParams ok args'
         exec (metaKernel ok) kerArgs
         let res = metaKernelResult ok kerArgs
         return (writeData res)

makeBuiltinIO :: IO FunctionalKernel -> MakeBuiltin
makeBuiltinIO ok readData writeData exec = do
   ok' <- ok
   makeBuiltin ok' readData writeData exec
   
