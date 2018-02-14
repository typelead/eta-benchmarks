import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import Control.Monad

main = do
  [n] <- fmap (fmap read) getArgs
  c <- newChan
  m <- newEmptyMVar
  a <- forkIO $ forM_ [1..n] $ \i -> writeChan c i
  b <- forkIO $ do
    forM_ [1..n] $ \i -> do
      i' <- readChan c
      when (i /= i') $
        error $ "Read: " ++ show i' ++ " Expected: " ++ show i
    putMVar m ()
  takeMVar m

