/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class SuperReservedWord extends BaseLiteral {

	private static final String SUPER = "super";
	public final int line;

	public SuperReservedWord(int line) {
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, line);
	}
}
