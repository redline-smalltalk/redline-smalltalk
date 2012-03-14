/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class PrimaryExpression extends Primary {

	private final Expression expression;

	PrimaryExpression(Expression expression) {
		this.expression = expression;
		expression.leaveResultOnStack();
	}

	int line() {
		return expression != null ? expression.line() : 0;
	}

    boolean isBlockWithAnswerExpression() {
        return false;
    }

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, line());
		if (expression != null)
			expression.accept(nodeVisitor);
	}
}
