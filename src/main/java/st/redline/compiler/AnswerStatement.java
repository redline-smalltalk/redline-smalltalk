/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class AnswerStatement extends Statements {

	private final int line;

	AnswerStatement(int line, Expression expression) {
		super(expression, null);
		this.line = line;
	}

	public int line() {
		return line;
	}
}
