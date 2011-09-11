/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmIntInsn extends JvmExpression {

	private final JvmOpcode jvmOpcode;
	private final int operand;

	public JvmIntInsn(JvmOpcode jvmOpcode, String operand) {
		this.jvmOpcode = jvmOpcode;
		this.operand = Integer.valueOf(operand);
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, jvmOpcode.value(), operand, jvmOpcode.line());
	}
}
