/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class ProtoBlock extends ProtoMethod {

	public ProtoBlock() {
		System.out.println("Block() instantiated " + this);
	}

	private RuntimeException subclassShouldHaveImplemented(int argumentCount) {
		return new IllegalStateException("A subclass of ProtoBlock should implement 'applyTo' with " + argumentCount + " arguments.");
	}
}
