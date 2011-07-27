/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline;

import java.util.HashMap;
import java.util.Map;

public class ProtoObject {

	private static final Map<String, ProtoObject> classRegistry = new HashMap<String, ProtoObject>();

	private Data data;

	public ProtoObject() {
		this(true);
	}

	public ProtoObject(boolean isClass) {
		data = isClass ? new ClassData() : new InstanceData();
	}

	public static void primitiveMain(ProtoObject receiver, String[] args) {
	}

	public static ProtoObject primitiveRegisterAs(ProtoObject receiver, String name) {
		classRegistry.put(name, receiver);
		return receiver;
	}

	public static ProtoObject primitiveVariableAt(ProtoObject receiver, String name) {
		System.out.println("primitiveVariableAt " + receiver + " " + name);
		if (primitiveIsInstanceVariable(receiver, name))
			throw new IllegalStateException("todo - implement");
		if (primitiveIsClassVariable(receiver, name))
			throw new IllegalStateException("todo - implement");
		return primitiveResolveObject(receiver, name);
	}

	public static boolean primitiveIsInstanceVariable(ProtoObject receiver, String name) {
		return false;
	}

	public static boolean primitiveIsClassVariable(ProtoObject receiver, String name) {
		return false;
	}

	public static ProtoObject primitiveSymbol(ProtoObject receiver, String value) {
		System.out.println("primitiveSymbol " + receiver + " " + value);
		ProtoObject symbolClass = receiver.resolveObject("Symbol");
		ProtoObject symbol = new ProtoObject(false);
		symbol.cls(symbolClass);
		symbol.javaValue(value);
		return symbol;
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, String selector, ProtoObject classMethodWasFoundIn) {
		System.out.println("selector " + selector);
		System.out.println("receiver " + receiver);
		System.out.println("receiver cls " + receiver.cls());
		System.out.println("classMtd " + classMethodWasFoundIn);
		throw new IllegalStateException("todo - implement " + selector);
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, ProtoObject arg1, String selector, ProtoObject classMethodWasFoundIn) {
		System.out.println("selector " + selector);
		System.out.println("receiver " + receiver);
		System.out.println("receiver cls " + receiver.cls());
		System.out.println("arg1 " + arg1);
		System.out.println("classMtd " + classMethodWasFoundIn);
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, receiver.cls(), arg1);
		ProtoObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyTo(receiver, newClassMethodWasFoundIn[0], arg1);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return primitiveSend(receiver, arg1, selector, classMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new ProtoObject[] {arg1});
	}

	private static boolean isBootstrapped(ProtoObject object) {
		return (object != null && (object.isBootstrapped() || object.cls().isBootstrapped()));
	}

	public ProtoObject ping() {
		System.out.println("here1 " + this + " " + this.cls() + " Proto  " + classRegistry.get("ProtoObject") + " symbol " + classRegistry.get("Symbol"));
		return this;
	}

	private static boolean isNotBootstrapped(ProtoObject object) {
		return !isBootstrapped(object);
	}

	private static boolean resolveClassObject(ProtoObject object) {
		return resolveClassObject(object, null);
	}

	private static boolean resolveClassObject(ProtoObject object, ProtoObject other) {
		System.out.println("resolveClassObject() " + object + " " + other);
		if (!resolveObjectSuperclass(object, other))
			return false;
		if (isNotBootstrapped(object))
			return true;
		return primitiveResolveObject(object, "Foo") != null;
	}

	private static boolean resolveObjectSuperclass(ProtoObject object, ProtoObject other) {
		System.out.println("resolveClassObjectSuperclass() " + object + " " + other);
		if (object == null)
			return true;
		if (!object.isClass())
			return resolveObjectSuperclass(object.cls(), other);
		ProtoObject superclass = object.superclass();
		if (superclass != other)
			return resolveClassObject(superclass, other);
		ProtoObject metaclass = object.cls().superclass();
		return resolveClassObject(metaclass, metaclass);
	}

	private static ProtoObject sendDoesNotUnderstand(ProtoObject receiver, String selector, ProtoObject[] arguments) {
		throw new RuntimeException("TODO -  need to implement send of doesNotUnderstand - '" + selector + "'");
	}

	private static ProtoMethod methodFor(ProtoObject rObject, String selector, ProtoObject[] classMethodFoundIn) {
		ProtoMethod method;
		ProtoObject superclass = rObject;
		while ((method = superclass.methodAt(selector)) == null)
			if ((superclass = superclass.superclass()) == null)
				break;
		classMethodFoundIn[0] = superclass;
		return method;
	}

	public static ProtoObject primitiveResolveObject(ProtoObject receiver, String name) {
		ProtoObject object = receiver.resolveObject(name);
		if (object != null)
			return object;
		// TODO.JCL search through namespaces to find object.
		return receiver.resolveObject("st.redline." + name);
	}

	private ProtoObject resolveObject(String name) {
		System.out.println("resolveObject() " + name);
		if (classRegistry.containsKey(name)) {
			System.out.println("from registry " + name);
			return classRegistry.get(name);
		}
		// It is expected the loading of an object results in the registering a Smalltalk class in the class registry.
		// *NOTE* if class is not registered the will be a NullPointerException as we return 'null' here.
		if (loadObject(name))
			return classRegistry.get(name);
		// TODO.JCL should we return 'nil'?
		throw new IllegalStateException("Handle this - Class not found.");
	}

	private boolean loadObject(String name) {
		try {
			return Class.forName(name, true, classLoader()).newInstance() != null;
		} catch (Exception e) {
			return false;
		}
	}

	private ClassLoader classLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	public void bootstrap() {
		new Bootstrapper(this).bootstrap();
	}

	public void bootstrapped() {
		data.bootstrapped();
	}

	public boolean isBootstrapped() {
		return data.isBootstrapped();
	}

	protected void cls(ProtoObject cls) {
		data.cls(cls);
	}

	protected ProtoObject cls() {
		return data.cls();
	}

	protected void superclass(ProtoObject superclass) {
		data.superclass(superclass);
	}

	protected ProtoObject superclass() {
		return data.superclass();
	}

	protected void javaValue(String value) {
		data.javaValue(value);
	}

	protected String javaValue() {
		return data.javaValue();
	}

	protected ProtoMethod methodAt(String selector) {
		return data.methodAt(selector);
	}

	protected void methodAtPut(String selector, ProtoMethod method) {
		data.methodAtPut(selector, method);
	}

	protected boolean isClass() {
		return data.isClass();
	}

	abstract class Data {

		private ProtoObject cls;
		private Map<String, ProtoObject> variables;
		private boolean bootstrapped = false;

		abstract void javaValue(String value);
		abstract String javaValue();
		abstract void superclass(ProtoObject superclass);
		abstract ProtoObject superclass();
		abstract ProtoMethod methodAt(String selector);
		abstract void methodAtPut(String selector, ProtoMethod method);
		abstract boolean isClass();

		protected void cls(ProtoObject cls) {
			this.cls = cls;
		}

		protected ProtoObject cls() {
			return cls;
		}

		protected void bootstrapped() {
			bootstrapped = true;
		}

		protected boolean isBootstrapped() {
			return bootstrapped;
		}
	}

	class InstanceData extends Data {

		private String javaValue;

		protected void javaValue(String value) {
			javaValue = value;
		}

		protected String javaValue() {
			return javaValue;
		}

		protected boolean isClass() {
			return false;
		}

		protected void superclass(ProtoObject superclass) {
			throw new IllegalStateException("An instance can't have a superclass.");
		}

		protected ProtoObject superclass() {
			throw new IllegalStateException("An instance doesn't have a superclass.");
		}

		protected ProtoMethod methodAt(String selector) {
			throw new IllegalStateException("An instance doesn't have a method dictionary.");
		}

		protected void methodAtPut(String selector, ProtoMethod method) {
			throw new IllegalStateException("An instance can't have a method dictionary.");
		}
	}

	class ClassData extends Data {

		private ProtoObject superclass;
		private Map<String, ProtoMethod> methods = new HashMap<String, ProtoMethod>();

		protected void javaValue(String value) {
			throw new IllegalStateException("A Class can't have a javaValue.");
		}

		protected String javaValue() {
			throw new IllegalStateException("A Class doesn't have a javaValue.");
		}

		protected boolean isClass() {
			return true;
		}

		protected void superclass(ProtoObject superclass) {
			this.superclass = superclass;
		}

		protected ProtoObject superclass() {
			return superclass;
		}

		protected ProtoMethod methodAt(String selector) {
			return methods.get(selector);
		}

		protected void methodAtPut(String selector, ProtoMethod method) {
			methods.put(selector, method);
		}
	}
}
