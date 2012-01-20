/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class AssignmentExpression implements Expression {

	private final String variableName;
	private final int line;
	private final Expression expression;

	AssignmentExpression(String variableName, int line, Expression expression) {
		this.variableName = variableName;
		this.line = line;
		this.expression = expression;
	}

	String variableName() {
		return variableName;
	}

	Expression expression() {
		return expression;
	}

	public int line() {
		return line;
	}

	public void accept(NodeVisitor nodeVisitor) {
	}
}
