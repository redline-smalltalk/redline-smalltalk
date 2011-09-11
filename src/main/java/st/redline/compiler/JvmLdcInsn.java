/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmLdcInsn extends JvmExpression {

	private final StringConstant stringConstant;

	public JvmLdcInsn(StringConstant stringConstant) {
		this.stringConstant = stringConstant;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, stringConstant.valueWithoutQuotes(), stringConstant.line);
	}
}
