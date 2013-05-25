/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.lang;

// Adds block functionality

import java.lang.reflect.InvocationTargetException;

public class ProtoBlock extends ProtoObject {

    protected static ProtoObject blockClosure;

    private boolean methodBlock = false;
    private PrimContext outerContext;

    public ProtoBlock() {
        this(null);
    }

    public ProtoBlock(PrimContext outerContext) {
        name = "Block";
        selfclass = resolveBlockClosure();
        this.outerContext = outerContext;
    }

    protected ProtoObject resolveBlockClosure() {
        if (blockClosure != null)
            return blockClosure;
        if (classLoader().isBootstrapping())
            return PrimNil.PRIM_NIL;
        blockClosure = resolveObject("st.redline.kernel.BlockClosure");
        return blockClosure;
    }

    public ProtoObject outerReceiver() {
        return outerContext.receiver;
    }

    public ProtoObject variableAt(String name) {
        int index = selfclass.indexOfVariable(name);
        if (index != 0)
            return attributes[index];
        return outerContext.receiver.variableAt(name);
    }

    public ProtoObject variableAtPut(String name, ProtoObject object) {
        int index = selfclass.indexOfVariable(name);
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

    public ProtoObject answer(ProtoObject answer, String blockReturnType) {
        if (notMethodBlock())
            throwAnswer(answer, blockReturnType);
        return answer;
    }

    public void throwAnswer(ProtoObject answer, String blockReturnType) {
        try {
//            System.out.println("throwAnswer: " + answer);
            throw (BlockReturn) classLoader()
                                    .loadBlockReturn(blockReturnType)
                                    .getConstructor(ProtoObject.class)
                                    .newInstance(answer);
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

}
