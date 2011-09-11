/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Primitive implements VisitableNode {

	private final String value;
	private final int line;

	public Primitive(String value, int line) {
		this.value = value;
		this.line = line;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, value, line);
	}
}
