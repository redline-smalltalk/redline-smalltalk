/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class ArrayConstant extends Primary {

	private Array array;
	private int line;

	ArrayConstant(Array array, int line) {
		this.array = array;
		this.line = line;
	}

	int line() {
		return line;
	}

	Array array() {
		return array;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, line);
		if (array != null)
			array().accept(nodeVisitor);
	}
}
