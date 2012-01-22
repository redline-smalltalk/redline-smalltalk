/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class PrimaryStatements extends Primary {

	private final Statements statements;

	PrimaryStatements(Statements statements) {
		this.statements = statements;
	}

	public void accept(NodeVisitor nodeVisitor) {
	}

	int line() {
		return statements != null ? statements.line() : 0;
	}
}
