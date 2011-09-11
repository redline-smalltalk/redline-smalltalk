/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class TrueReservedWord extends BaseLiteral {

	private static final String TRUE = "TRUE";
	public final int line;

	public TrueReservedWord(int line) {
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, line);
	}
}
