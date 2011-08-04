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
import java.util.Stack;

public class ProtoObject {

	private static final Map<String, ProtoObject> classRegistry = new HashMap<String, ProtoObject>();
	private static final ThreadLocal<Stack<String>> packageRegistry = new ThreadLocal<Stack<String>>();
	protected static final Map<String, String> packageMap = new HashMap<String, String>();

	protected static ProtoObject instanceOfUndefinedObject;
	protected static ProtoObject instanceOfTrue;
	protected static ProtoObject instanceOfFalse;

	private ProtoObjectData data;

	public ProtoObject() {
		this(true);
	}

	public ProtoObject(boolean isClass) {
		data = isClass ? ProtoObjectData.classData() : ProtoObjectData.instanceData();
	}

	public static ProtoObject primitiveMain(ProtoObject receiver, String[] args) {
		return receiver;
	}

	public static String primitivePackageRegistryCurrent() {
		Stack<String> stack = packageRegistry.get();
		if (stack != null)
			return stack.peek();
		return "";
	}

	public static void primitivePackageRegistryCurrent(String packageName) {
		Stack<String> stack = packageRegistry.get();
		if (stack == null) {
			stack = new Stack<String>();
			packageRegistry.set(stack);
		}
		stack.push(packageName.replaceAll("/", "."));
	}

	public static void primitivePackageRegistryRemove() {
		Stack<String> stack = packageRegistry.get();
		if (stack != null)
			stack.pop();
	}

	public static ProtoObject primitiveCreateSubclass(ProtoObject receiver, String name) {
		System.out.println("primitiveCreateSubclass() " + receiver + " " + name);
		if (name != null && classRegistry.containsKey(name))
			return classRegistry.get(name);
		return createSubclass(createClass(receiver), createMetaclass(receiver));
	}

	private static ProtoObject createSubclass(ProtoObject aClass, ProtoObject metaclass) {
		aClass.cls(metaclass);
		return aClass;
	}

	private static ProtoObject createClass(ProtoObject receiver) {
		ProtoObject cls = new ProtoObject();
		cls.superclass(receiver);
		return cls;
	}

	private static ProtoObject createMetaclass(ProtoObject receiver) {
		ProtoObject metaclass = new ProtoObject();
		metaclass.cls(ProtoObject.primitiveResolveObject(receiver, "MetaClass"));  // TODO.JCL Should this be 'Metaclass new'?
		metaclass.superclass(receiver.cls());
		return metaclass;
	}

	public static ProtoObject primitiveRegisterAs(ProtoObject receiver, String name) {
		System.out.println("primitiveRegisterAs() " + String.valueOf(name) + " " + receiver);
		classRegistry.put(name, receiver);
		return receiver;
	}

	public static ProtoObject primitiveVariableAt(ProtoObject receiver, String name) {
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
		ProtoObject symbolClass = receiver.resolveObject("Symbol");  // <- should we do primitiveVariableAt so namespaces are used?
		ProtoObject symbol = new ProtoObject(false);
		symbol.cls(symbolClass);
		symbol.javaValue(value);
		return symbol;
	}

	public static ProtoObject primitiveString(ProtoObject receiver, String value) {
		ProtoObject stringClass = receiver.resolveObject("String");  // <- should we do primitiveVariableAt so namespaces are used?
		ProtoObject string = new ProtoObject(false);
		string.cls(stringClass);
		string.javaValue(value);
		return string;
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, String selector, ProtoObject classMethodWasFoundIn) {
		throw new IllegalStateException("todo - implement " + selector);
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, ProtoObject arg1, String selector, ProtoObject classMethodWasFoundIn) {
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

//	public ProtoObject ping() {
//		System.out.println("here1 " + this + " " + this.cls() + " Proto  " + classRegistry.get("ProtoObject") + " symbol " + classRegistry.get("Symbol"));
//		return this;
//	}

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

	private static ProtoMethod methodFor(ProtoObject object, String selector, ProtoObject[] classMethodFoundIn) {
		ProtoMethod method;
		ProtoObject superclass = object;
		while ((method = superclass.methodAt(selector)) == null)
			if ((superclass = superclass.superclass()) == null)
				break;
		classMethodFoundIn[0] = superclass;
		return method;
	}

	public static ProtoObject primitiveResolveObject(ProtoObject receiver, String name) {
		System.out.println("primitiveResolveObject() " + name);
		ProtoObject object = receiver.resolveObject(name);
		if (object != null)
			return object;
		// TODO.JCL should we return 'nil'?
		throw new IllegalStateException("Class '" + name + "' not found.");
	}

	private ProtoObject resolveObject(String name) {
		if (classRegistry.containsKey(name))
			return classRegistry.get(name);
		// TODO.JCL include looking in receivers own packageMap.
		String fullyQualifiedName = ProtoObject.primitivePackageFor(this, name);
		if (fullyQualifiedName != null)
			return primitiveResolveObject(this, fullyQualifiedName);
		// It is expected the loading of an object results in the registering a Smalltalk class in the class registry.
		// *NOTE* if class is not registered the will be a NullPointerException as we return 'null' here.
		if (loadObject(name))
			return classRegistry.get(name);
		return null;
	}

	public static String primitivePackageFor(ProtoObject receiver, String name) {
		String fullyQualifiedName = receiver.packageFor(name);
		if (fullyQualifiedName != null)
			return fullyQualifiedName;
		return packageMap.get(name);
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

	private String packageFor(String name) {
		if (isClass())
			return data.packageFor(name);
		ProtoObject cls = cls();
		if (cls != null)
			return packageFor(name);
		return null;
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

	public void cls(ProtoObject cls) {
		data.cls(cls);
	}

	public ProtoObject cls() {
		return data.cls();
	}

	public void superclass(ProtoObject superclass) {
		data.superclass(superclass);
	}

	public ProtoObject superclass() {
		return data.superclass();
	}

	public void javaValue(String value) {
		data.javaValue(value);
	}

	public String javaValue() {
		return data.javaValue();
	}

	public ProtoMethod methodAt(String selector) {
		return data.methodAt(selector);
	}

	public void methodAtPut(String selector, ProtoMethod method) {
		data.methodAtPut(selector, method);
	}

	public boolean isClass() {
		return data.isClass();
	}
}
