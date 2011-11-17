/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class ProtoBlock extends ProtoMethod {

	private ThisContext thisContext;

	public ProtoBlock() {
		try {
			superclass(Primitives.resolveObject(this, "st.redline.Object"));
			cls(Primitives.resolveObject(this, "st.redline.BlockClosure"));
		} catch (ClassNotFoundException e) {
			throw new RedlineException(e);
		}
	}

	protected RuntimeException subclassShouldHaveImplemented(int argumentCount) {
		return new IllegalStateException("A subclass of ProtoBlock should implement 'applyTo' with " + argumentCount + " arguments.");
	}

	public void outerContext(ThisContext thisContext) {
		this.thisContext = thisContext;
	}

	public ThisContext outerContext() {
		return thisContext;
	}
}
