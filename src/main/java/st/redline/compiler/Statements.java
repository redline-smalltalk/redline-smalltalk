/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Statements implements VisitableNode {

	private final Expression expression;
	private final Statements statements;

	Statements(Expression expression, Statements statements) {
		this.expression = expression;
		this.statements = statements;
		if (statements == null && expression != null)
			expression.leaveResultOnStack();
	}

	int line() {
		return expression.line();
	}

	Expression expression() {
		return expression;
	}

	Statements statements() {
		return statements;
	}

    boolean hasAnswerExpression() {
        return expression.isAnswerExpression() || (statements != null && statements.hasAnswerExpression());
    }

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visitBegin(this);
		if (expression != null)
			expression.accept(nodeVisitor);
		if (statements != null)
			statements.accept(nodeVisitor);
		nodeVisitor.visitEnd(this);
	}
}
