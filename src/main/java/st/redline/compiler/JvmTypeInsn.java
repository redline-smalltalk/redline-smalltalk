/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmTypeInsn extends JvmExpression {

	private final JvmOpcode jvmOpcode;
	private final StringConstant type;

	public JvmTypeInsn(JvmOpcode jvmOpcode, StringConstant type) {
		this.jvmOpcode = jvmOpcode;
		this.type = type;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, jvmOpcode.value(), type.valueWithoutQuotes(), jvmOpcode.line());
	}
}
