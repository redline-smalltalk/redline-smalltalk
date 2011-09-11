/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class AnswerExpression implements Expression {

	private final Expression expression;

	public AnswerExpression(Expression expression) {
		this.expression = expression;
	}

	public void leaveResultOnStack() {
		expression.leaveResultOnStack();
	}

	public void duplicateResultOnStack() {
		throw new IllegalStateException("Answer expression asked to duplicate stack top!");
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		leaveResultOnStack();
		expression.accept(visitor);
	}
}
