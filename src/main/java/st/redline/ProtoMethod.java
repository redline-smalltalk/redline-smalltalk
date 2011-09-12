/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class ProtoMethod extends ProtoObject {

	public ProtoMethod() {
		super(false);
	}

	private RuntimeException subclassShouldHaveImplemented(int argumentCount) {
		return new IllegalStateException("A subclass of ProtoMethod should implement 'applyTo' with " + argumentCount + " arguments.");
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls) {
		throw subclassShouldHaveImplemented(0);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1) {
		throw subclassShouldHaveImplemented(1);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1, ProtoObject arg2) {
		throw subclassShouldHaveImplemented(2);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3) {
		throw subclassShouldHaveImplemented(3);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4) {
		throw subclassShouldHaveImplemented(4);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5) {
		throw subclassShouldHaveImplemented(5);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6) {
		throw subclassShouldHaveImplemented(6);
	}

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject cls, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		throw subclassShouldHaveImplemented(7);
	}
}
