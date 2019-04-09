{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Control.Concurrent.Fiber.MVar
  (MVar, takeMVar, putMVar)
where

import GHC.Base
import Control.Concurrent.Fiber
import Control.Monad.IO.Class
import GHC.MVar (MVar(..))

takeMVar :: MVar a -> Fiber a
takeMVar (MVar m) = callCC go
  where go k = do
          mResult <- liftIO $ tryTake
          case mResult of
            Just a  -> k a
            Nothing -> block >> go k

        tryTake = IO $ \s ->
          case tryTakeMVar# m s of
            (# s', 0#, _ #) ->
              case addMVarListener# m s' of
                s'' -> (# s'', Nothing #)
            (# s', _,  a #) ->
              case awakenMVarListeners# m s' of
                s'' -> (# s'', Just a #)

putMVar :: MVar a -> a -> Fiber ()
putMVar (MVar m) x = callCC go
  where go k = do
          success <- liftIO $ tryPut
          if success
          then k ()
          else block >> go k

        tryPut = IO $ \s ->
          case tryPutMVar# m x s of
            (# s', 0# #) ->
              case addMVarListener# m s' of
                s'' -> (# s'', False #)
            (# s', _  #) ->
              case awakenMVarListeners# m s' of
                s'' -> (# s'', True #)

 

foreign import prim "eta.fibers.PrimOps.addMVarListener"
  addMVarListener# :: MVar# s a -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.awakenMVarListeners"
  awakenMVarListeners# :: MVar# s a -> State# s -> State# s
