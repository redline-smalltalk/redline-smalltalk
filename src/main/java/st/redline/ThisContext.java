/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class ThisContext {

	public ProtoObject classMethodFoundIn;

	private ProtoObject[] temporaries;
	private ProtoObject[] arguments;

	public ThisContext(ProtoObject classMethodFoundIn) {
		this.classMethodFoundIn = classMethodFoundIn;
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg};
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg1, ProtoObject arg2) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg1, arg2};
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg1, arg2, arg3};
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg1, arg2, arg3, arg4};
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg1, arg2, arg3, arg4, arg5};
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg1, arg2, arg3, arg4, arg5, arg6};
	}

	public ThisContext(ProtoObject classMethodFoundIn, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6, ProtoObject arg7) {
		this.classMethodFoundIn = classMethodFoundIn;
		arguments = new ProtoObject[] {arg1, arg2, arg3, arg4, arg5, arg6, arg7};
	}

	public ThisContext() {
		this(null);
	}

	public ProtoObject argumentAt(int index) {
		if (arguments == null || arguments.length < index)
			throw new IllegalStateException("Invalid access of method argument at index " + index);
		return arguments[index];
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
