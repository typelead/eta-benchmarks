package eta.fibers;

import java.util.Queue;
import java.util.Map;
import java.util.Stack;
import java.util.IdentityHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.concurrent.Fiber;
import eta.runtime.concurrent.MVar;
//import eta.runtime.exception;

import static ghc_prim.ghc.Types.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.Closures.*;

/* TODO: Provide cleanup operations by extending the runtime with hooks. */


public class PrimOps {
    public static class EmptyException extends eta.runtime.exception.StgException {}
    public static final EmptyException EMPTYEXCEPTION= new EmptyException();

    public static IdentityHashMap<TSO,Closure> tsoEvent = new IdentityHashMap<TSO,Closure>();
    public static void throwEmpty(StgContext context){
        throw EMPTYEXCEPTION;
    }
    public static Closure alternativeFiber(StgContext context, Closure fa, Closure fb) {
        TSO tso = context.currentTSO;
        int oldTop = tso.contStackTop;
        try {
            return fa.applyV(context);
        } catch (EmptyException e) {
            tso.contStackTop = oldTop;
               tso.currentCont= fb;
            return fb.applyV(context);
        }
    }

    

    public static int topStackC(StgContext context){
        return context.currentTSO.contStackTop;

    }



    public static Object getStackC(StgContext context){
        Closure[] newContStack = new Closure[context.currentTSO.contStackTop];
        System.arraycopy(context.currentTSO.contStack,0,newContStack,0, context.currentTSO.contStackTop);
        return newContStack;
    }


    public static void setTopStackC(StgContext context,int top){
        context.currentTSO.contStackTop= top;
    }


    public static TSO getTSOC(StgContext context) {
        return context.currentTSO;
    }

    public static void setContStack(StgContext context, int top, Object newContStack,Closure current){ 
        context.currentTSO.contStack= (Closure[]) newContStack;
        context.currentTSO.contStackTop= top;
        context.currentTSO.currentCont = current;

    }
    
    public static Closure getEventCC(StgContext context){
        Closure v= tsoEvent.get(context.currentTSO);
        if (v==null)  context.I1 = 0; else context.I1 = 1;
        return v;
    }
    public static void setEventC(StgContext context,Closure ev){
        tsoEvent.put(context.currentTSO,ev);
    }
    
    public static void delEventCC(StgContext context){
        tsoEvent.remove(context.currentTSO);
    }



    public static void setCurrentC(StgContext context, Closure action) {
        context.currentTSO.currentCont = action;
    }

    public static void pushNextC(StgContext context, Closure action) {
        context.currentTSO.pushCont(action);
    }

    public static Closure popNextC(StgContext context) {
        return context.currentTSO.popCont();
    }

    public static Closure getCurrentC(StgContext context) {
        return context.currentTSO.currentCont;
    }

    public static Closure popContStack(StgContext context) {
        TSO tso = context.currentTSO;
        if (tso.emptyContStack()) {
            context.I1 = 0;
            return null;
        } else {
            context.I1 = 1;
            return tso.popCont();
        }
    }

    public static Closure resumeFiber = null;

    static {
        try {
          resumeFiber = loadClosure("main.Internal", "resumeFiber");
        } catch (Exception e) {
            System.err.println("FATAL ERROR: Failed to load resumeFiber closure.");
            e.printStackTrace();
            System.exit(1);
        }
    }
    public static void yieldFiber(StgContext context, int block) {
        TSO tso = context.currentTSO;
        tso.whatNext = (block == 1)? ThreadBlock : ThreadYield;
        Closure oldClosure = tso.closure;
        if (oldClosure instanceof EvalLazyIO) {
            ((EvalLazyIO) oldClosure).p = resumeFiber;
        } else {
            oldClosure = Closures.evalLazyIO(resumeFiber);
        }
        throw Fiber.yieldException.get();
    }

    public static void addMVarListener(StgContext context, MVar m) {
        m.registerListener(context.currentTSO);
    }

    public static void awakenMVarListeners(StgContext context, MVar m) {
        for (TSO top = m.getListeners(); top != null;) {
            Concurrent.pushToGlobalRunQueue(top);
            TSO oldTop = top;
            top = top.link;
            oldTop.link = null;
        }
    }
}
