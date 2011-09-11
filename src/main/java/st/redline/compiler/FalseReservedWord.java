/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class FalseReservedWord extends BaseLiteral {

	private static final String FALSE = "FALSE";
	public final int line;

	public FalseReservedWord(int line) {
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, line);
	}
}
