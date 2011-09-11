/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmInsn extends JvmExpression {

	private final JvmOpcode jvmOpcode;

	public JvmInsn(JvmOpcode jvmOpcode) {
		this.jvmOpcode = jvmOpcode;
	}

	public void accept(NodeVisitor nodeVisitor) {
		if (jvmOpcode != null)
			nodeVisitor.visit(this, jvmOpcode.value(), jvmOpcode.line());
	}
}
