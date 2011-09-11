/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Program implements VisitableNode {

	private final Temporaries temporaries;
	private final Statements statements;
	private final Methods methods;

	public Program(Temporaries temporaries, Statements statements, Methods methods) {
		this.temporaries = temporaries;
		this.statements = statements;
		this.methods = methods;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		if (temporaries != null)
			temporaries.accept(visitor);
		if (statements != null)
			statements.accept(visitor);
		if (methods != null)
			methods.accept(visitor);
		visitor.visitEnd(this);
	}
}
