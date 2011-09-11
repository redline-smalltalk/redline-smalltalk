/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Keyword implements VisitableNode {

	protected final String value;
	protected final int line;

	public Keyword(String value, int line) {
		this.value = value;
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}
}
