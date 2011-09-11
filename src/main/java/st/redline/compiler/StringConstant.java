/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class StringConstant implements VisitableNode {

	protected final String value;
	protected final int line;

	public StringConstant(String value, int line) {
		this.value = value;
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}

	public String valueWithoutQuotes() {
		if (value.charAt(0) == '\'')
			return value.substring(1, value.length() - 1);
		return value;
	}
}
