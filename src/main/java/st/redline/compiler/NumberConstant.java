/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class NumberConstant extends BaseLiteral {

	protected final String value;
	protected final int line;
	private int index = 0;

	public NumberConstant(String value, int line) {
		this.value = value;
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}

	public int line() {
		return line;
	}

	public String value() {
		return value;
	}

	public void index(int index) {
		this.index = index;
	}

	public int index() {
		return index;
	}
}
