/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class ThisContext {

	public ProtoObject classMethodFoundIn;

	private ProtoObject[] temporaries;

	public ThisContext(ProtoObject classMethodFoundIn) {
		this.classMethodFoundIn = classMethodFoundIn;
	}

	public ThisContext() {
		this(null);
	}

	public ProtoObject temporaryAt(int index) {
		return temporaries[index];
	}

	public void temporaryAtPut(int index, ProtoObject value) {
		temporaries[index] = value;
	}

	public void temporariesInit(int size) {
		temporaries = new ProtoObject[size];
	}
}
