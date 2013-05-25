/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class ArrayConstant extends Primary {

	private Array array;
	private int line;

    public ArrayConstant(Array array, int line) {
		this.array = array;
		this.line = line;
	}

    public int line() {
		return line;
	}

    public Array array() {
		return array;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, line);
		if (array != null)
			array().accept(nodeVisitor);
	}
}
