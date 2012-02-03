/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// Provides a simple primitive object that knows how to:
// - perform a message
// - hold a java value object
// - hold 'n' + 1 primitive objects, one is class object
// Typically you don't create instances of PrimObject directly, instead you ask
// a PrimObjectClass for an instance of the class it represents by sending it the 'new' message.

import st.redline.compiler.Block;

import java.util.Map;
import java.util.Stack;
import java.util.concurrent.ConcurrentHashMap;

public class PrimObject {

	public static final Stack<String> PACKAGE_REGISTRY = new Stack<String>();
	public static final Map<String, PrimObject> CLASSES = new ConcurrentHashMap<String, PrimObject>();
	static final Map<Object, Object> INTERNED_SYMBOLS = new ConcurrentHashMap<Object, Object>();
	static final Map<String, PrimObject> BLOCKS = new ConcurrentHashMap<String, PrimObject>();
	static final PrimObject NIL = null;

	static final PrimObject TRUE = null;
	static final PrimObject FALSE = null;
	static final int DEFAULT_ATTRIBUTE_COUNT = 1;  // default attribute is class

	static final int CLASS_INDEX = 0;
	static final PrimObject BASIC_DOES_NOT_UNDERSTAND = new PrimBasicDoesNotUnderstand();
	static final PrimObject PRIM_NIL = new PrimObject();

	static boolean BOOTSTRAPPING = false;

	Object javaValue = null;
	PrimObject[] attributes;

	public PrimObject() {
		this(0);
	}

	PrimObject(int basicSize) {
		attributes = new PrimObject[basicSize + DEFAULT_ATTRIBUTE_COUNT];
		initialize();
	}

	public PrimObject block(String name) {
		if (BLOCKS.containsKey(name))
			return createBlockInstance(BLOCKS.get(name));
		Block block = (Block) SmalltalkClassLoader.BLOCKS_TO_BE_COMPILED.remove(name);
		if (block == null)
			throw new IllegalStateException("Block to be compiled '" + name + "' not found.");
		block.accept(block.analyser());
		try {
			PrimObject newblock = (PrimObject) smalltalkClassLoader().defineClass(block.classBytes()).newInstance();
			BLOCKS.put(name, newblock);
			return newblock;
		} catch (Exception e) {
			throw new RedlineException(e);
		}
	}

	SmalltalkClassLoader smalltalkClassLoader() {
		return (SmalltalkClassLoader) Thread.currentThread().getContextClassLoader();
	}

	static PrimObject createBlockInstance(PrimObject block) {
		try {
			// We don't create a new instance because the context passed to a block contains space for args and temps.
			// Nb: first thing a block's invoke methods does is set the context temporary space.
			return block;
		} catch (Exception e) {
			throw new RedlineException(e);
		}
	}

	public static PrimObject string(Object javaValue) {
		PrimObject newObject = new PrimObject();
		newObject.javaValue = javaValue;
		return newObject;
	}

	public static PrimObject number(Object javaValue) {
		throw new IllegalStateException("implement me");
	}

	public static PrimObject character(Object javaValue) {
		throw new IllegalStateException("implement me");
	}

	public static PrimObject symbol(Object javaValue) {
		Object internedValue = intern(javaValue);
		if (BOOTSTRAPPING) {
			PrimObject newObject = new PrimObject();
			newObject.javaValue = internedValue;
			return newObject;
		}
		throw new IllegalStateException("implement me");
	}

	static Object intern(Object value) {
		if (INTERNED_SYMBOLS.containsKey(value))
			return INTERNED_SYMBOLS.get(value);
		INTERNED_SYMBOLS.put(value, value);
		return value;
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
		if (CLASSES.containsKey(name))
			return CLASSES.get(name);
		if (Character.isUpperCase(name.charAt(0))) {
			String fullyQualifiedName = packageFor(name);
			if (fullyQualifiedName != null)
				return resolveObject(fullyQualifiedName);
		}
		// It is expected the loading of an object results in the registering of
		// a Smalltalk class in the class registry (CLASSES).
		PrimObject primObject = loadObject(name);
		if (primObject != null) {
			if (CLASSES.containsKey(name))
				return CLASSES.get(name);
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

	protected PrimObject invoke(PrimObject receiver, PrimContext context) {
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

	public Object javaValue() {
		return javaValue;
	}

	public PrimObject javaValue(Object javaValue) {
		this.javaValue = javaValue;
		return this;
	}

	public PrimObject cls() {
		return attributes[CLASS_INDEX];
	}

	PrimObject cls(PrimObject aClass) {
		attributes[CLASS_INDEX] = aClass;
		return this;
	}
}
