/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmFieldInsn extends JvmExpression {

	private final JvmOpcode jvmOpcode;
	private final StringConstant owner;
	private final StringConstant name;
	private final StringConstant description;

	public JvmFieldInsn(JvmOpcode jvmOpcode, StringConstant owner, StringConstant name, StringConstant description) {
		this.jvmOpcode = jvmOpcode;
		this.owner = owner;
		this.name = name;
		this.description = description;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, jvmOpcode.value(), owner.valueWithoutQuotes(), name.valueWithoutQuotes(), description.valueWithoutQuotes(), jvmOpcode.line());
	}
}
