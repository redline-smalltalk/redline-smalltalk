/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class AssignmentExpression implements Expression {

	private final Identifier identifier;
	private final Expression expression;

	AssignmentExpression(Identifier identifier, Expression expression) {
		this.identifier = identifier;
		this.identifier.onStoreSideOfExpression();
		this.expression = expression;
		this.expression.leaveResultOnStack();
	}

	String variableName() {
		return identifier.value();
	}

	Expression expression() {
		return expression;
	}

	public int line() {
		return identifier.line();
	}

	public void leaveResultOnStack() {
		expression.duplicateResultOnStack();
	}

	public void duplicateResultOnStack() {
		throw new IllegalStateException("Assignment asked to duplicate stack top!");
	}

    public boolean isAnswerExpression() {
        return false;
    }

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visitBegin(this);
		// Note: The order of visiting below is important. Expression before variable we store result into.
		if (expression != null) {
			expression.accept(nodeVisitor);
		}
		if (identifier != null)
			identifier.accept(nodeVisitor);
		nodeVisitor.visitEnd(this);
	}
}
