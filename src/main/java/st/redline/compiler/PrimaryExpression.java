/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class PrimaryExpression extends BasePrimary {

	protected final Expression expression;

	public PrimaryExpression(Expression expression) {
		this.expression = expression;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
	}
}
