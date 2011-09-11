/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmIincInsn extends JvmExpression {

	private final int variable;
	private final int increment;
	private final int line;

	public JvmIincInsn(String variable, String increment, int line) {
		this.variable = Integer.valueOf(variable);
		this.increment = Integer.valueOf(increment);
		this.line = line;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, variable, increment, line);
	}
}
