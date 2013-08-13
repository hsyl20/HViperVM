module ViperVM.VirtualPlatform.Transfer where

-- | Allocate a new instance, transfer appropriate data from another one, then
-- associate the new instance
allocateTransferAttach :: VirtualObject -> Object -> Memory -> IO Object
allocateTransferAttach so src dstMem = do
   let 
      pf = virtualMemoryPlatform som
      lks = linksBetween srcMem dstMem (links pf)
      indirectTransfer = Prelude.null lks

   -- Allocate destination
   dst <- allocateInstance som so dstMem


   -- Intermediate steps
   steps <- if indirectTransfer
      then do
         -- Use host memory as intermediate
         let hostMem = head . hostMemories $ virtualMemoryPlatform som
         ho <- allocateInstance som so hostMem 
         return [ho,dst]
      else return [dst]

   let 
      f s d = do
         transferObject om s d
         atomically (attachInstance so d)
         return d

   -- Perform transfer
   foldM f src steps


-- | Ensure that an instance of a shared object is in a given memory. perform a transfer if necessary
ensureInMemory :: Memory -> VirtualObject -> IO Object
ensureInMemory mem so = do
   -- Try to retrieve a direct instance
   Set.elems <$> atomically (instancesInMemory so mem) >>= \case
      x:_ -> return x
      [] -> do
         -- Try to retrieve a linked instance
         Set.elems <$> atomically (linkedInstancesInMemory so mem) >>= \case
            x:_ -> return x
            [] -> do

               -- Perform transfer
               Set.elems <$> atomically (allInstances so) >>= \case
                  [] -> error "Uninitialized object accessed in read-only mode"
                  src:_ -> allocateTransferAttach som so src mem
                  -- FIXME: select source policy is not clever
