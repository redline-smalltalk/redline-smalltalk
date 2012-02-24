/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// Adds block functionality

public class PrimObjectBlock extends PrimObject {

	public PrimObjectBlock() {
		this(0);
	}

	PrimObjectBlock(int basicSize) {
		super(basicSize);
		System.out.println("** Block created **");
	}
}
