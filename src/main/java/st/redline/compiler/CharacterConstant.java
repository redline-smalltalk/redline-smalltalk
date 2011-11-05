/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class CharacterConstant implements IndexedVisitableNode {

	protected final String value;
	protected final int line;
	private int index = 0;

	public CharacterConstant(String value, int line) {
		this.value = value;
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}

	public void index(int index) {
		this.index = index;
	}

	public int index() {
		return index;
	}
}
