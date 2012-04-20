/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

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
		if (PrimObject.BOOTSTRAPPING)
			return PRIM_NIL;
		blockClosure = resolveObject("st.redline.BlockClosure");
		return blockClosure;
	}

	PrimObject variableAtPut(String name, PrimObject object) {
		System.out.println("Block variableAtPut() " + name + " " + object);
		int index = cls().indexOfVariable(name);
		if (index != 0) {
			attributes[index] = object;
			return this;
		}
		System.out.println("OuterContext: " + outerContext + " OuterReceiver: " + outerContext.receiver);
		throw new IllegalStateException("Slot for '" + name + "' not found.");
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
