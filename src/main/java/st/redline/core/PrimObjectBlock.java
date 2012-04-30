/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

// Adds block functionality

import java.lang.reflect.InvocationTargetException;

public class PrimObjectBlock extends PrimObject {

    static PrimObject blockClosure;

    private boolean methodBlock = false;
    private PrimContext outerContext;

    public PrimObjectBlock() {
        this(0, null);
    }

    public PrimObjectBlock(PrimContext outerContext) {
        this(0, outerContext);
    }

    PrimObjectBlock(int basicSize, PrimContext outerContext) {
        super(basicSize);
        cls(resolveBlockClosure());
        this.outerContext = outerContext;
    }

    PrimObject resolveBlockClosure() {
        if (blockClosure != null)
            return blockClosure;
        if (BOOTSTRAPPING)
            return PRIM_NIL;
        blockClosure = resolveObject("st.redline.core.BlockClosure");
        return blockClosure;
    }

    public PrimObject outerReceiver() {
        return outerContext.receiver;
    }

    public PrimObject variableAt(String name) {
        int index = cls().indexOfVariable(name);
        if (index != 0)
            return attributes[index];
        return outerContext.receiver.variableAt(name);
    }

    PrimObject variableAtPut(String name, PrimObject object) {
        int index = cls().indexOfVariable(name);
        if (index != 0) {
            attributes[index] = object;
            return this;
        }
        outerContext.receiver.variableAtPut(name, object);
        return this;
    }

    public PrimContext outerContext() {
        return outerContext;
    }

    public void markAsMethodBlock() {
        methodBlock = true;
    }

    public boolean isMethodBlock() {
        return methodBlock;
    }

    public boolean notMethodBlock() {
        return !isMethodBlock();
    }

    public PrimObject answer(PrimObject answer, String blockReturnType) {
        if (notMethodBlock())
            throwAnswer(answer, blockReturnType);
        return answer;
    }

    public void throwAnswer(PrimObject answer, String blockReturnType) {
        try {
            throw (BlockReturn) SmalltalkClassLoader.instance()
                                     .loadClass(blockReturnType)
                                     .getConstructor(PrimObject.class)
                                     .newInstance(answer);
        } catch (InstantiationException e) {
            throw new RedlineException(e);
        } catch (IllegalAccessException e) {
            throw new RedlineException(e);
        } catch (InvocationTargetException e) {
            throw new RedlineException(e);
        } catch (NoSuchMethodException e) {
            throw new RedlineException(e);
        } catch (ClassNotFoundException e) {
            throw new RedlineException(e);
        }
    }
}
