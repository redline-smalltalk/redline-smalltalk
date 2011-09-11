/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class NumberConstant extends BaseLiteral {

	protected final String value;
	protected final int line;

	public NumberConstant(String value, int line) {
		this.value = value;
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}

	@Override
	public int line() {
		return line;
	}

	@Override
	public String value() {
		return value;
	}
}
