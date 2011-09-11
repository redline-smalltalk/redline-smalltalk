/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class InstanceMethod extends AbstractMethod {

	public InstanceMethod(String objectName, MessagePattern messagePattern, Primitive primitive, Temporaries temporaries, Statements statements) {
		super(objectName, messagePattern, primitive, temporaries, statements);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		super.accept(visitor);
		visitor.visitEnd(this);
	}
}
