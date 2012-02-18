/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.util.HashMap;
import java.util.Map;

// Adds a method dictionary and class hierarchy (superclass) to a Primitive Object.
// To create an instance of the Class this object represents send it a 'new' message.
// Typically you don't create instances of PrimObjectClass directly, instead you ask
// a PrimObjectMetaclass for an instance of the class it represents.
// NOTE: DONT EVER CREATE AN INSTANCE OF THIS CLASS DIRECTLY - SEE OBJECTMETACLASS.

public class PrimObjectClass extends PrimObject {

	static final int DEFAULT_ATTRIBUTE_COUNT = 1;  // for superclass
	static final int SUPERCLASS_INDEX = PrimObject.CLASS_INDEX + 1;

	PrimObjectClass() {
		this(0);
	}

	PrimObjectClass(int basicSize) {
		super(basicSize + DEFAULT_ATTRIBUTE_COUNT);
		javaValue = new HashMap<String, PrimObject>();
	}

	int indexOf(String name) {
		System.out.println("Class indexOf() " + name);
		return 0;
	}

	public String packageFor(String name) {
		PrimObject cls = cls();
		if (cls != null)
			cls.packageFor(name);
		return null;
	}

	boolean includesSelector(String selector) {
		return methods().containsKey(selector);
	}

	PrimObject methodFor(String selector) {
		return methods().get(selector);
	}

	public Map<String, PrimObject> methods() {
		return (Map<String, PrimObject>) javaValue;
	}

	public PrimObject superclass() {
		return attributes[SUPERCLASS_INDEX];
	}

	PrimObject superclass(PrimObject superclass) {
		attributes[SUPERCLASS_INDEX] = superclass;
		return this;
	}
}
