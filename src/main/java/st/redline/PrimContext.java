/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class PrimContext {

	public PrimObject receiver;
	public PrimObject lookupClass;
	public String selector;
	public PrimObject[] arguments;
	private PrimObject[] temporaries;

	public PrimContext(PrimObject receiver) {
		this.receiver = receiver;
	}

	public PrimContext(PrimObject receiver, PrimObject lookupClass, String selector, PrimObject ... arguments) {
		this.receiver = receiver;
		this.lookupClass = lookupClass;
		this.selector = selector;
		this.arguments = arguments;
	}

	PrimObject[] temporaries() {
		return temporaries;
	}

	public PrimObject temporaryAt(int index) {
		return temporaries[index];
	}

	public PrimObject argumentAt(int index) {
		return arguments[index];
	}

	public void temporariesInit(int size) {
		temporaries = new PrimObject[size];
		for (int i = 0; i < size; i++)
			temporaries[i] = PrimObject.NIL;
	}
}
