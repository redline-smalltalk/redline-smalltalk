/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.math.BigInteger;
import java.util.*;

// To create a new class, you need an instance of Metaclass, which you build up and then 'new'.
// The newed object IS the class that gets registered.

public class ProtoObject {

	protected static final Map<String, String> packageMap = new HashMap<String, String>();
	protected static final Map<String, ProtoObject> classRegistry = classRegistryMapInstance();

	public static ProtoObject METACLASS_INSTANCE;  // <- sole instance of class Metaclass.
	public static ProtoObject TRUE;
	public static ProtoObject FALSE;
	public static ProtoObject NIL;

	private ProtoObject cls;
	private ProtoObject superclass;
	private Object javaValue;
	private Map<String, ProtoMethod> methods;
	private Map<String, String> packages;
	private Map<String, ProtoObject> variables;
	private Map<String, ProtoObject> instanceVariables;

	private String name;

	public void name(String name) {
		this.name = name;
	}

	public String name() {
		return name;
	}

	public String toString() {
		if(javaValue() instanceof String) {
			return (String)javaValue();
		} else {
			if (javaValue() instanceof Number) {
				return ((Number)javaValue()).toString();
			}
			if (name != null) return name;
			if (cls() != null)
				return super.toString() + "(" + String.valueOf(cls().name) + ")";
			return super.toString();
		}
	}

	public ProtoObject() {
		initialize();
	}

	public ProtoObject(ProtoObject cls) {
		this.cls = cls;
		initialize();
	}

	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (!(o instanceof ProtoObject))
			return false;
		ProtoObject other = (ProtoObject) o;
		if (javaValue != null)
			return javaValue.equals(other.javaValue);
		return false;
	}

	public boolean isBlock() {
		return false;
	}

	protected static ProtoObject resolveObject(ProtoObject receiver, String name) throws ClassNotFoundException {
		return receiver.resolveObject(name);
	}

	private void initialize() {
		initializeInstanceVariables(cls(), variablesMapInstance());
	}

	protected void initializeInstanceVariables(ProtoObject cls, Map<String, ProtoObject> variables) {
		if (cls != null) {
//			System.out.println("initializeInstanceVariables() " + cls + " " + cls.superclass());
			if (cls.instanceVariables() != null) {
				for (Map.Entry<String, ProtoObject> entry : cls.instanceVariables().entrySet())
					variables.put(entry.getKey(), NIL);
				variables(variables);
			}
			if (cls.superclass() != null) {
				if (cls.superclass() == cls)
					throw new IllegalStateException("Class and superclass are same object.");
				initializeInstanceVariables(cls.superclass(), variables);
			}
		}
	}

	public void bootstrap() throws ClassNotFoundException {
		new Bootstrapper(this).bootstrap();
	}

	public ProtoObject(BigInteger BigInteger) {
		javaValue(BigInteger);
	}

	protected ProtoObject registerAs(String name) {
		classRegistry.put(name, this);
		return this;
	}

	protected ProtoObject resolveObject(String name) throws ClassNotFoundException {
//		System.out.println("resolveObject() " + name);
		ProtoObject object = resolveObject0(name);
		if (object != null)
			return object;
		throw new ClassNotFoundException(name);
	}

	private ProtoObject resolveObject0(String name) throws ClassNotFoundException {
		if (classRegistry.containsKey(name))
			return classRegistry.get(name);

		if (Character.isUpperCase(name.charAt(0))) {
			String fullyQualifiedName = packageAt(name);
			if (fullyQualifiedName != null)
				return resolveObject(fullyQualifiedName);
		}

		// It is expected the loading of an object results in the registering a Smalltalk class in the class registry.
		// *NOTE* if class is not registered the will be a NullPointerException as we return 'null' here.
		ProtoObject object = loadObject(name);
		if (object != null) {
			if (classRegistry.containsKey(name))
				return classRegistry.get(name);
			// We loaded a class and created an instance, it may be a script so return it.
			return object;
		}
		return null;
	}

	protected ProtoObject loadObject(String name) {
//		System.out.println("loadObject() " + name);
		try {
			return (ProtoObject) Class.forName(name, true, classLoader()).newInstance();
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
	}

	public ThisContext outerContext() {
		throw new IllegalStateException("Subclass should implement.");
	}

	public ProtoObject outerReceiver() {
		throw new IllegalStateException("Subclass should implement.");
	}

	private ClassLoader classLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	protected ProtoMethod methodAt(String name) {
		if (methods != null && methods.containsKey(name))
			return methods.get(name);
		ProtoObject superclass = superclass();
		while (superclass != null) {
			if (superclass.methods != null && superclass.methods.containsKey(name))
				return superclass.methods.get(name);
			superclass = superclass.superclass();
		}
		return null;
	}

	protected ProtoObject methodAtPut(String name, ProtoMethod method) {
		if (methods == null)
			methods = methodsMapInstance();
		methods.put(name, method);
		return this;
	}

	protected boolean hasMethod(String name) {
		return methods != null && methods.containsKey(name);
	}

	public ProtoObject category(String category) {
		return this;
	}

	public String packageAt(String name) {
		if (packages != null) {
			String fullyQualifiedName = packages.get(name);
			if (fullyQualifiedName != null)
				return fullyQualifiedName;
		}
		return packageMap.get(name);
	}

	public ProtoObject packageAtPut(String name, String javaPackageName) {
		if (packages == null)
			packages = packageMapInstance();
		packages.put(name, javaPackageName.replace("/", "."));
		return this;
	}

	public ProtoObject javaValue(Object javaValue) {
		this.javaValue = javaValue;
		return this;
	}

	public Object javaValue() {
		return javaValue;
	}

	protected ProtoObject cls(ProtoObject cls) {
		this.cls = cls;
		return this;
	}

	public ProtoObject cls() {
		return cls;
	}

	public ProtoObject send(String selector) {
		return Primitives.send(this, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject argument) {
		return Primitives.send(this, argument, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject firstArgument, ProtoObject secondArgument) {
		return Primitives.send(this, firstArgument, secondArgument, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject firstArgument, ProtoObject secondArgument, ProtoObject thirdArgument) {
		return Primitives.send(this, firstArgument, secondArgument, thirdArgument, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject firstArgument, ProtoObject secondArgument, ProtoObject thirdArgument, ProtoObject fourthArgument) {
		return Primitives.send(this, firstArgument, secondArgument, thirdArgument, fourthArgument, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject firstArgument, ProtoObject secondArgument, ProtoObject thirdArgument, ProtoObject fourthArgument, ProtoObject fifthArgument) {
		return Primitives.send(this, firstArgument, secondArgument, thirdArgument, fourthArgument, fifthArgument, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject firstArgument, ProtoObject secondArgument, ProtoObject thirdArgument, ProtoObject fourthArgument, ProtoObject fifthArgument, ProtoObject sixthArgument) {
		return Primitives.send(this, firstArgument, secondArgument, thirdArgument, fourthArgument, fifthArgument, sixthArgument, selector, null);
	}

	public ProtoObject send(String selector, ProtoObject firstArgument, ProtoObject secondArgument, ProtoObject thirdArgument, ProtoObject fourthArgument, ProtoObject fifthArgument, ProtoObject sixthArgument, ProtoObject seventhArgument) {
		return Primitives.send(this, firstArgument, secondArgument, thirdArgument, fourthArgument, fifthArgument, sixthArgument, seventhArgument, selector, null);
	}

	protected void superclass0(ProtoObject superclass) {
		this.superclass = superclass;
	}

	protected ProtoObject superclass(ProtoObject superclass) {
		this.superclass = superclass;
		copySuperclassClassInstanceVariables(this, superclass);
		if (cls != null && cls != METACLASS_INSTANCE && superclass != null) {
			cls.superclass = superclass.cls();
			copySuperclassClassInstanceVariables(cls, superclass.cls());
		}
		return this;
	}

	private void copySuperclassClassInstanceVariables(ProtoObject receiver, ProtoObject superclass) {
		if (superclass != null && superclass.cls() != null && superclass.cls().variables() != null)
			for (Map.Entry<String, ProtoObject> entry : superclass.cls().variables().entrySet())
				Primitives.addClassInstanceVariable(receiver, entry.getKey(), false);
	}

	protected ProtoObject superclass() {
		return superclass;
	}

	protected ProtoObject variableAt(String name) {
//		System.out.println("variableAt() " + this + " " + name + " " + (variables != null ? variables.get(name) : null));
		return variables != null ? variables.get(name) : null;
	}

	protected ProtoObject variableAtPut(String name, ProtoObject value) {
		return variables.put(name, value);
	}

	protected ProtoObject variables(Map<String, ProtoObject> variables) {
		this.variables = variables;
		return this;
	}

	protected Map<String, ProtoObject> variables() {
		return variables;
	}

	public boolean hasVariableNamed(String variable) {
		return (variables() != null && variables().containsKey(variable))
				|| (superclass() != null && superclass().hasVariableNamed(variable));
	}

	protected ProtoObject instanceVariables(Map<String, ProtoObject> instanceVariables) {
		this.instanceVariables = instanceVariables;
		return this;
	}

	protected Map<String, ProtoObject> instanceVariables() {
		return instanceVariables;
	}

	public boolean hasInstanceVariableNamed(String variable) {
		return (instanceVariables() != null && instanceVariables().containsKey(variable))
				|| (superclass() != null && superclass().hasInstanceVariableNamed(variable));
	}

	public static Map<String, ProtoObject> classRegistryMapInstance() {
		return new Hashtable<String, ProtoObject>();
	}

	public static Map<String, ProtoObject> variablesMapInstance() {
		return new Hashtable<String, ProtoObject>();
	}

	public static Map<String, ProtoMethod> methodsMapInstance() {
		return new Hashtable<String, ProtoMethod>();
	}

	public static Map<String, String> packageMapInstance() {
		return new Hashtable<String, String>();
	}
}
