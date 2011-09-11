/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class PrimaryStatements extends BasePrimary {

	protected final Statements statements;

	public PrimaryStatements(Statements statements) {
		this.statements = statements;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
	}
}
