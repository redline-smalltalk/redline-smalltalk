/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public abstract class AbstractMethod implements Method {

	protected final String objectName;
	protected final MessagePattern messagePattern;
	protected final Primitive primitive;
	protected final Temporaries temporaries;
	protected final Statements statements;
	protected final boolean empty;

	public AbstractMethod(String objectName, MessagePattern messagePattern, Primitive primitive, Temporaries temporaries, Statements statements) {
		this.objectName = objectName;
		this.messagePattern = messagePattern;
		this.primitive = primitive;
		this.temporaries = temporaries;
		this.statements = statements;
		this.empty = primitive == null && statements == null;
	}

	public boolean isEmpty() {
		return empty;
	}

	public String objectName() {
		return objectName;
	}

	public int line() {
		return messagePattern.line();
	}

	public void accept(NodeVisitor visitor) {
		messagePattern.accept(visitor);
		// we don't visit the rest during class analysis, but we do during method analysis.
		if (!visitor.continueMethodVisit())
			return;
		if (primitive != null)
			primitive.accept(visitor);
		if (temporaries != null)
			temporaries.accept(visitor);
		if (statements != null)
			statements.accept(visitor);
	}
}
