/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class ThisContext {

	public ProtoObject classMethodFoundIn;

	public ThisContext(ProtoObject classMethodFoundIn) {
		this.classMethodFoundIn = classMethodFoundIn;
	}

	public ThisContext() {
		this(null);
	}

	public ProtoObject temporaryAt(int index) {
		System.out.println("ThisContext.temporaryAt() " + index);
		return null;
	}

	public void temporaryAtPut(int index, ProtoObject value) {
		System.out.println("ThisContext.temporaryAtPut() " + index + " " + value);
	}
}
