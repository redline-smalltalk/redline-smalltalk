/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class PrimaryExpression extends Primary {

	private final Expression expression;

	PrimaryExpression(Expression expression) {
		this.expression = expression;
	}

	public void accept(NodeVisitor nodeVisitor) {
	}

	int line() {
		return expression != null ? expression.line() : 0;
	}
}
