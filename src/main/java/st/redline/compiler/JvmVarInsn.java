/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmVarInsn extends JvmExpression {

	private final JvmOpcode jvmOpcode;
	private final int number;

	public JvmVarInsn(JvmOpcode jvmOpcode, String number) {
		this.jvmOpcode = jvmOpcode;
		this.number = Integer.valueOf(number);
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, jvmOpcode.value(), number, jvmOpcode.line());
	}
}
