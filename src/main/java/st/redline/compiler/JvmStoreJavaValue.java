/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmStoreJavaValue extends JvmExpression {

	private final int line;

	public JvmStoreJavaValue(int line) {
		this.line = line;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, line);
	}
}
