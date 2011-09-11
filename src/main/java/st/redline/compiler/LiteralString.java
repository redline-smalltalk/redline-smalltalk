/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class LiteralString extends BaseLiteral {

	public final String value;
	public final int line;

	public LiteralString(StringConstant stringConstant) {
		value = stringConstant.value;
		line = stringConstant.line;
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
}
