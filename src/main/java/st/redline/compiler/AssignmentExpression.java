/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class AssignmentExpression implements Expression {

	private final VariableName variableName;
	private final Expression expression;

	public AssignmentExpression(VariableName variableName, Expression expression) {
		this.variableName = variableName;
		this.expression = expression;
	}

	public void leaveResultOnStack() {
		expression.duplicateResultOnStack();
	}

	public void duplicateResultOnStack() {
		throw new IllegalStateException("Assignment asked to duplicate stack top!");
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		expression.leaveResultOnStack();
		expression.accept(visitor);
		variableName.onStoreSideOfExpression();
		variableName.accept(visitor);
	}
}
