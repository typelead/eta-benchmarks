module Internal where

import GHC.Base
import GHC.Conc.Sync
import Control.Monad
import Unsafe.Coerce

import Data.Typeable
import Control.Concurrent
import Control.Exception
import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid hiding (Any)
import Java.Core
import Data.IORef
-- Fiber
import Debug.Trace 
-- (!>) x y= trace (show y) x


newtype Fiber a = Fiber { unFiber :: State# RealWorld -> (# State# RealWorld, a #) }

instance Functor Fiber where
  fmap f (Fiber io) = Fiber $ \s -> case io s of (# s1, a #) -> (# s1, f a #)


data Empty= Empty  deriving  (Show)
instance Exception Empty

instance Alternative Fiber where
  empty= Fiber $ \s -> case throwEmpty# s of s1 -> (# s1, undefined #)
  mf <|> mg= unsafeCoerce $ Fiber $ \s ->  alternativeFiber#  (unsafeCoerce mf) (unsafeCoerce mg) s
    --   Fiber $ \s -> case topStack#  s of
    -- (# s1, top, current #)  -> catch# mf (mg' top current) s1
    -- where
    -- mg' top current Empty s = case setTopStack# top (unsafeCoerce mg) s of
    --     s1 -> mg s1 
catchEmpty :: Monad m => m a -> m a -> m a
catchEmpty mf mg= unsafeCoerce $ Fiber $ \s ->  alternativeFiber#  (unsafeCoerce mf) (unsafeCoerce mg) s
throwEmpty :: IO a
throwEmpty =  IO $ \s -> case throwEmpty# s of s1 -> (# s1, undefined #)

instance Monoid a => Monoid (Fiber a) where
  mempty= return mempty
  mappend x y=  (<>) <$> x <*> y 

instance Applicative Fiber where
  pure = return
  -- (<*>) = ap
  mf <*> mx = do 
    r1 <- liftIO $ newIORef Nothing
    r2 <- liftIO $ newIORef Nothing
    fparallel r1 r2 <|>  xparallel r1 r2 
    where
    fparallel r1 r2= do 
      f <- mf 
      liftIO $ (writeIORef r1 $ Just f) 
      mx <- liftIO (readIORef r2) 
      case mx of 
        Nothing -> empty 
        Just x  -> return $ f x 

    xparallel r1 r2 = do 
      x <- mx 
      liftIO $ (writeIORef r2 $ Just x)
      mf <- liftIO (readIORef r1)
      case mf of
        Nothing -> empty 
        Just f -> return $ f x

  -- Fiber mf <*> Fiber mx = Fiber $ \s ->  
  --   case newMutVar# Nothing s  of
  --     (# s1, r1 #) -> case newMutVar# Nothing s1 of
  --       (# s2, r2 #)  ->  case topStack#  s2 of
  --         (# s3, top #)  ->  catch# (fparallel r1 r2  )     ( xparallel r1 r2 top ) s3
  
  --   where
    
  --   fparallel r1 r2 s=  case mf s of
  --         (# s3, f #)  ->  case writeMutVar# r1 (Just f) s3  of
  --           s4#  -> case readMutVar# r2   s4#  of
  --             (# s5,mx #)  -> case mx of
  --               Just x  -> (# s5, f x #)
  --               Nothing -> raiseIO# (toException Empty) s5
                
    
  --   xparallel  r1 r2 top (_ :: Empty) s=  case mx s of
  --         (# s3, x #)  ->  case writeMutVar# r2 (Just x)  s3  of
  --           s4#  -> case readMutVar# r1   s4#  of
  --             (# s5,mf #)  -> case mf of
  --               Just f  -> (# s5, f x #)
  --               Nothing -> case setTopStack# top s5 of
  --                 s6 -> raiseIO# (toException Empty) s6
                


instance Monad Fiber where
  return :: a -> Fiber a
  return a = Fiber $ \s -> (# s, a #)

  (>>=) :: forall a b. Fiber a -> (a -> Fiber b) -> Fiber b
  (>>=) (Fiber m) f = Fiber $ \s ->
    case setCurrentC# (unsafeCoerce m) s of
      s1 -> case pushNextC# (unsafeCoerce f) s1 of
        s2 -> case m s2 of
          (# s3, a #) -> case popNextC# s3 of
            (# s4, _ #) ->
              case f a of
                fa -> case setCurrentC# (unsafeCoerce fa) s4 of
                  s5 -> unFiber fa s5

instance MonadIO Fiber where
  liftIO :: IO a -> Fiber a
  liftIO (IO m) = Fiber m

-- Fiber Utilities
runFiber :: forall a. Fiber a -> IO (Either (Fiber a) a)
runFiber (Fiber m) = undefined
  -- catch (fmap Right $ IO m) (\(Yield _ fiber) -> return $ Left (unsafeCoerce fiber))

runFiberWithBlock :: forall a. Fiber a -> IO (Either (Bool, Fiber a) a)
runFiberWithBlock (Fiber m) = undefined
--   catch (fmap Right $ IO m) $
--   \(Yield block fiber) -> return $ Left (block, unsafeCoerce fiber)

resumeFiber :: Fiber ()
resumeFiber = Fiber $ \s ->
  case getCurrentC# s of
    (# s1, fiber #) ->
      case (unsafeCoerce fiber) s1 of
        (# s2, a #) -> (# go a s2, () #)
  where go :: Any -> State# s -> State# s
        go a s =
          case popContStack# s of
            (# s1, 1#, cont1 #) ->
              let fa = (unsafeCoerce cont1) a
              in case setCurrentC# fa s1 of
                   s2 -> case fa s2 of
                     (# s3, a' #) -> go a' s3
            (# s1, _, _ #) -> s1

react
    :: Typeable eventdata
    => ((eventdata ->  IO response) -> IO ())
    -> IO  response
    -> Fiber eventdata
react setHandler iob= liftIO $ do
              mev <- getEvent
              case mev of
                Nothing -> do
                    I# top <- topStack
                    Obj stack <- getStack
                    curr  <- getCurrent
                    setHandler $ \dat ->do
                                    forkCont (return dat) top stack curr  -- make sure TSO info is in 
                                    iob
                    throwEmpty
      
                Just ev -> do
                    delEvent
                    return $ unsafeCoerce ev
        
    where
    

async :: IO a -> Fiber a 
async io= liftIO $ do 
    mev <- getEvent 
    case mev of

      Nothing -> do 
                forkCont' io
                throwEmpty 
      Just ev -> do 
            delEvent
            return ev
    
    where
    forkCont' io= do 
        I# top <- topStack
        Obj stack <- getStack
        curr  <- getCurrent
        forkCont io top stack curr 

forkCont io top stack curr= do
        forkIO' $ do
          setTSO  top   stack curr 
          ev <- io
          setEvent ev 
          
          unlift resumeFiber  
          return () 
         `catchEmpty`  return ()
            
        return()
     where
     setTSO   top stack current  = IO $ \s -> case setContStack# top stack current  s of s2 ->  (# s2, () #)
      
     forkIO' (IO mx)= IO $ \s -> case fork# mx s of  (# s1, tid #) -> (# s1, ThreadId tid #)
     
topStack= IO $ \s -> case topStack# s of (#s1, i #) -> (# s1, I# i #)  
getStack= IO $ \s -> case getStack# s of (#s1, arr #) -> (#s1, Obj arr #) 
getCurrent= IO $ \s -> getCurrentC# s

data Obj = Obj (Object# Object)

unlift (Fiber fib)= IO fib
     

-- async1 :: IO a -> Fiber a 
-- async1 (IO io)=  Fiber $ \s -> io' s
--     where
--     unFiber (Fiber fib)= fib
--     io' s =  case getEvent# s  of
--       (# s2, 0#, _ #) -> case forkCont s2  of
--                 (# s5, _ #) ->  raiseIO# (toException Empty) s5
      
--       (# s2, _, x #) -> case delEvent#  s2 of 
--                   s3 -> (# s3, x #)
  
--     forkCont = \s ->   case getTSO# s of (#s2, tso #) -> fork# (execCont tso) s2
--       where   
--       execCont tso  =IO $ \s -> case io s of
--         (# s1, x #) ->  case setEvent#  (unsafeCoerce  x) s1 of
--            s2 ->  case setContStack# tso s1 of s2 -> alternativeFiber#  (unsafeCoerce resumeFiber) 
--                                                                         (unsafeCoerce $ (return() ::IO())) s2

      
            

yield :: Fiber a
yield = yield' False

block :: Fiber a
block = yield' True

yield' :: Bool -> Fiber a
yield' block = Fiber $ \s ->
  case popContStack# s of
    (# s1, 1#, current #) ->
      let fa = (unsafeCoerce current) extractYieldError
      in case setCurrentC# (unsafeCoerce fa) s1 of
           s2 -> (# yieldFiber# (dataToTag# block) s2
                 ,  unreachableCodeError #)
    (# s1, _, _ #) -> (# s1, lastYieldError #)
  where extractYieldError =
          error "Attempted to extract a value from a Fiber's yield or block."
        lastYieldError =
          error "You cannot yield or block as the last action of a Fiber."
        unreachableCodeError =
          error "This code should not have been reached."

forkFiber x= forkFiber'  x `catchEmpty`   myThreadId
  where
  forkFiber' :: Fiber () -> IO ThreadId
  forkFiber' (Fiber m)= IO $ \s ->
    case fork# m s of (# s1, tid #) -> (# s1, ThreadId tid #)


setEvent x= IO $ \s -> case setEvent# (unsafeCoerce  x) s of s1 -> (#s1 , () #)

getEvent :: IO (Maybe a)
getEvent = IO $ \s -> case getEvent#  s of
      (# s1, 1# , x #) -> (#s1, Just $ unsafeCoerce x #)
      (# s1,  _ , _ #) -> (#s1, Nothing #)

delEvent= IO $ \s -> case delEvent# s of  s1->  (# s1,() #)


catchf :: Exception e => Fiber a -> (e -> Fiber a) -> Fiber a
catchf (Fiber exp)  exc=  
    case IO exp `catch` (\e -> case exc e  of  Fiber r -> IO r) of
        (IO r) -> Fiber r


-- -- pure state

-- data State= State  (State# RealWorld)
-- getData :: Typeable a => Fiber (Maybe a)
-- getData = resp
--   where 
--   -- get :: Fiber [(TypeRep,())]
--   get= Fiber  $ \s -> (# s,  State s #) 
    
--   resp = do
--           st <- get
--           let list= unsafeCoerce st
--           case lookup (typeOf $ typeResp resp) list of
--             Just x  -> return . Just $ unsafeCoerce x
--             Nothing -> return Nothing
--   typeResp :: m (Maybe x) -> x
--   typeResp = undefined
-- -- Runtime primitives
-- setData x =  do
--     st' <- get
--     let st= unsafeCoerce st'
--     let nelem=  (t ,unsafeCoerce x)
--     put $ State $ nelem : filter ((/=) t . fst) st
--     where 
--     t = typeOf x
--     get= Fiber  $ \s -> (# s, State s #) 
--     put (State x)= Fiber $ \_ -> (# x,() #)
    
data {-# CLASS "java.util.Stack" #-} Stack

type Stack# = Object# Stack

foreign import prim "eta.fibers.PrimOps.getCurrentC"
  getCurrentC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.setCurrentC"
  setCurrentC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.pushNextC"
  pushNextC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.popNextC"
  popNextC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.popContStack"
  popContStack# :: State# s -> (# State# s, Int#, Any #)

foreign import prim "eta.fibers.PrimOps.yieldFiber"
  yieldFiber# :: Int# -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.getEventCC"
  getEvent# :: State# s -> (# State# s, Int#, a #)

foreign import prim "eta.fibers.PrimOps.setEventC"
  setEvent# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.delEventCC"
  delEvent# ::  State# s -> State# s

foreign import prim "eta.fibers.PrimOps.getTSOC"
   getTSO# ::  State# s -> (# State# s, ThreadId# #)

foreign import prim "eta.fibers.PrimOps.setContStack"
   setContStack# ::  Int# -> Object# Object -> Any -> State# s  -> State# s

-- foreign import prim "eta.fibers.PrimOps.traceC"
--    trace# ::  String -> State# s  -> State# s

foreign import prim "eta.fibers.PrimOps.topStackC"
   topStack# ::  State# s  -> (#State# s, Int# #)

foreign import prim "eta.fibers.PrimOps.getStackC"
   getStack# ::  State# s  -> (#State# s,  Object# Object #)




foreign import prim "eta.fibers.PrimOps.setTopStackC"
   setTopStack# ::  Int# ->  State# s  -> State# s

foreign import prim "eta.fibers.PrimOps.alternativeFiber"
   alternativeFiber# ::  Any -> Any -> State# s  -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.throwEmpty"
   throwEmpty# ::  State# s  -> State# s
