/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// Adds block functionality

public class PrimObjectBlock extends PrimObject {

	static PrimObject blockClosure;

	public PrimObjectBlock() {
		this(0);
	}

	PrimObjectBlock(int basicSize) {
		super(basicSize);
		cls(resolveBlockClosure());
	}

	PrimObject resolveBlockClosure() {
		if (blockClosure != null)
			return blockClosure;
		if (PrimObject.BOOTSTRAPPING)
			return PRIM_NIL;
		blockClosure = resolveObject("st.redline.BlockClosure");
		return blockClosure;
	}
}
