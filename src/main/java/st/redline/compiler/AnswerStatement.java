/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class AnswerStatement extends Statements {

	private final int line;

	AnswerStatement(int line, Expression expression) {
		super(expression, null);
		this.line = line;
		expression.leaveResultOnStack();
	}

	public int line() {
		return line;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visitBegin(this);
		if (expression() != null)
			expression().accept(nodeVisitor);
		nodeVisitor.visitEnd(this);
	}
}
