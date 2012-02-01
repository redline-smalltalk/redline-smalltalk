/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class CharacterConstant extends ValuePrimary implements ArrayElement {

	private int index;
	private boolean insideArray = false;

	CharacterConstant(String value, int line) {
		super(value, line);
	}

	public void insideArray() {
		insideArray = true;
	}

	public boolean isInsideArray() {
		return insideArray;
	}

	public int index() {
		return index;
	}

	public void index(int index) {
		this.index = index;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, value(), index, line());
	}
}
