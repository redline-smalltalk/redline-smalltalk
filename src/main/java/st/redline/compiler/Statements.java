/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Statements implements VisitableNode {

	private final Expression expression;
	private final Statements statements;

	public Statements(Expression expression) {
		this.expression = expression;
		this.statements = null;
	}

	public Statements(Expression expression, Statements statements) {
		this.expression = expression;
		this.statements = statements;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		if (statements == null)
			expression.leaveResultOnStack();
		expression.accept(visitor);
		visitor.visitEnd(this);
		if (statements != null) {
			statements.accept(visitor);
		}
	}
}
