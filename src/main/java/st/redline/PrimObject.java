/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// Provides a simple primitive object that knows how to:
// - perform a message
// - hold a java value object
// - hold 'n' + 1 primitive objects, one is class object
// Typically you don't create instances of PrimObject directly, instead you ask
// a PrimObjectClass for an instance of the class it represents by sending it the 'new' message.

import java.util.Hashtable;
import java.util.Map;

public class PrimObject {

	static final Map<String, PrimObject> classes = new Hashtable<String, PrimObject>();

	static final PrimObject NIL = null;
	static final PrimObject TRUE = null;
	static final PrimObject FALSE = null;

	static final int DEFAULT_ATTRIBUTE_COUNT = 1;  // default attribute is class
	static final int CLASS_INDEX = 0;
	static final PrimObject BASIC_DOES_NOT_UNDERSTAND = new PrimBasicDoesNotUnderstand();
	static final PrimObject PRIM_NIL = new PrimObject();

	Object javaValue = null;
	PrimObject[] attributes;

	PrimObject() {
		this(0);
	}

	PrimObject(int basicSize) {
		attributes = new PrimObject[basicSize + DEFAULT_ATTRIBUTE_COUNT];
		initialize();
	}

	public static PrimObject string(Object javaValue) {
		PrimObject newObject = new PrimObject();
		newObject.javaValue = javaValue;
		return newObject;
	}

	public static void dump(Object object) {
		System.out.println("Dump: " + object);
		for (int i = 0; i < ((PrimObject) object).attributes.length; i++)
			System.out.println(i + " " + ((PrimObject) object).attributes[i]);
	}

	public PrimObject variableAt(String name) {
		return resolveObject(name);
	}

	PrimObject resolveObject(String name) {
		if (classes.containsKey(name))
			return classes.get(name);
		if (Character.isUpperCase(name.charAt(0))) {
			String fullyQualifiedName = packageFor(name);
			if (fullyQualifiedName != null)
				return resolveObject(fullyQualifiedName);
		}
		// It is expected the loading of an object results in the registering of
		// a Smalltalk class in the class registry (classes).
		PrimObject primObject = loadObject(name);
		if (primObject != null) {
			if (classes.containsKey(name))
				return classes.get(name);
			return primObject;
		}
		throw new IllegalStateException("Line should never be reached.");
	}

	PrimObject loadObject(String name) {
		try {
			return (PrimObject) Class.forName(name, true, classLoader()).newInstance();
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
	}

	ClassLoader classLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	public String packageFor(String name) {
		PrimObject cls = cls();
		if (cls != null)
			return cls.packageFor(name);
		return null;
	}

	// NOTE: Having perform bundle arguments and call perform0 simplifies the call process in the compiler.
	// A future optimisation will be to remove this bundling, as creating an array and setting values is
	// expensive.

	public PrimObject perform(String selector) {
		return perform0(selector);
	}

	public PrimObject perform(PrimObject arg1, String selector) {
		return perform0(selector, arg1);
	}

	public PrimObject perform(PrimObject arg1, PrimObject arg2, String selector) {
		return perform0(selector, arg1, arg2);
	}

	public PrimObject perform(PrimObject arg1, PrimObject arg2, PrimObject arg3, String selector) {
		return perform0(selector, arg1, arg2, arg3);
	}

	public PrimObject perform(PrimObject arg1, PrimObject arg2, PrimObject arg3, PrimObject arg4, String selector) {
		return perform0(selector, arg1, arg2, arg3, arg4);
	}

	public PrimObject perform(PrimObject arg1, PrimObject arg2, PrimObject arg3, PrimObject arg4, PrimObject arg5, String selector) {
		return perform0(selector, arg1, arg2, arg3, arg4, arg5);
	}

	PrimObject perform0(String selector, PrimObject ... arguments) {
		return perform0(attributes[CLASS_INDEX], selector, arguments);
	}

	void initialize() {
		for (int i = 0; i < attributes.length; i++)
			attributes[i] = PRIM_NIL;
	}

	PrimObject perform0(PrimObject foundInClass, String selector, PrimObject ... arguments) {
		PrimObject cls = foundInClass;
		while (!cls.includesSelector(selector))
			cls = cls.superclass();
		return apply(cls.methodFor(selector), cls, selector, arguments);
	}

	PrimObject apply(PrimObject method, PrimObject foundInClass, String selector, PrimObject ... arguments) {
		return method.invoke(this, new PrimContext(this, foundInClass, selector, arguments));
	}

	PrimObject invoke(PrimObject receiver, PrimContext context) {
		return this;
	}

	boolean includesSelector(String selector) {
		return true;
	}

	PrimObject methodFor(String selector) {
		return BASIC_DOES_NOT_UNDERSTAND;
	}

	public PrimObject superclass() {
		return this;
	}

	Object javaValue() {
		return javaValue;
	}

	PrimObject javaValue(Object javaValue) {
		this.javaValue = javaValue;
		return this;
	}

	PrimObject cls() {
		return attributes[CLASS_INDEX];
	}

	PrimObject cls(PrimObject aClass) {
		attributes[CLASS_INDEX] = aClass;
		return this;
	}
}
