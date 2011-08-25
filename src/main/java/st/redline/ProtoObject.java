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

import st.redline.compiler.AbstractMethod;
import st.redline.compiler.MethodAnalyser;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public class ProtoObject {

	private static final Map<String, AbstractMethod> methodsToBeCompiled = new HashMap<String, AbstractMethod>();
	private static final Map<String, ProtoObject> classRegistry = new HashMap<String, ProtoObject>();
	private static final ThreadLocal<Stack<String>> packageRegistry = new ThreadLocal<Stack<String>>();
	protected static final Map<String, String> packageMap = new HashMap<String, String>();	private static final Map<String, ProtoObject> symbols = new HashMap<String, ProtoObject>();

	protected static ProtoObject instanceOfUndefinedObject;
	protected static ProtoObject instanceOfTrue;
	protected static ProtoObject instanceOfFalse;

	private ProtoObjectData data;
	private String name;
	
	public ProtoObject() {
		this(true);
	}

	public ProtoObject(String name) {
		this(true);
		this.name = name;
	}

	public ProtoObject(boolean isClass) {
		data = isClass ? ProtoObjectData.classData() : ProtoObjectData.instanceData();
	}

	public ProtoObject(ProtoObject cls) {
		this(false);
		cls(cls);
	}

	public void name(String name) {
		this.name = name;
	}

	public String toString() {
		if (name != null) return name;
		if (cls() != null)
			return super.toString() + "(" + String.valueOf(cls().name) + ")";
		return super.toString();
	}

	public static void registerMethodToBeCompiledAs(AbstractMethod method, String name) {
		if (methodsToBeCompiled.containsKey(name))
			throw new IllegalStateException("Method to be compiled registered twice: " + name);
		methodsToBeCompiled.put(name, method);
	}

	private static ProtoObject createArray(ProtoObject receiver) {
		return create(receiver, "st.redline.Array");
	}

	private static ProtoObject create(ProtoObject receiver, String name) {
		ProtoObject cls = primitiveResolveObject(receiver, name);
		// System.out.println(cls);
		return primitiveSend(cls, "new", null);
	}

	public static String primitivePackageRegistryCurrent() {
		Stack<String> stack = packageRegistry.get();
		if (stack != null)
			return stack.peek();
		return "";
	}

	public static void primitiveCompileMethod(ProtoObject receiver, String fullMethodName, String methodName, String className, String packageName, int countOfArguments) {
		// TODO.JCL clean this up.
		// System.out.println("primitiveCompileMethod() " + receiver + " " + fullMethodName + " " + methodName + " " + className + " " + packageName + " " + countOfArguments);
		AbstractMethod methodToBeCompiled = methodsToBeCompiled.remove(fullMethodName);
		if (methodToBeCompiled == null)
			throw new IllegalStateException("Method to be compiled '" + fullMethodName + "' not found.");
		MethodAnalyser methodAnalyser = new MethodAnalyser(className + '$' + methodName, packageName, countOfArguments);
		methodToBeCompiled.accept(methodAnalyser);
		Class methodClass = ((SmalltalkClassLoader) Thread.currentThread().getContextClassLoader()).defineClass(methodAnalyser.classBytes());
		ProtoMethod method;
		try {
			method = (ProtoMethod) methodClass.newInstance();
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
		receiver.methodAtPut(methodName, method);
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
		// System.out.println("primitiveCreateSubclass() " + receiver + " " + name);
		if (name != null && classRegistry.containsKey(name))
			return classRegistry.get(name);
		return createSubclass(createClass(receiver, name), createMetaclass(receiver, name));
	}

	private static ProtoObject createSubclass(ProtoObject aClass, ProtoObject metaclass) {
		aClass.cls(metaclass);
		return aClass;
	}

	private static ProtoObject createClass(ProtoObject receiver, String name) {
		ProtoObject cls = new ProtoObject(name);
		cls.superclass(receiver);
		return cls;
	}

	private static ProtoObject createMetaclass(ProtoObject receiver, String name) {
		ProtoObject metaclass = new ProtoObject(name + " Metaclass");
		metaclass.cls(ProtoObject.primitiveResolveObject(receiver, "st.redline.MetaClass"));  // TODO.JCL Should this be 'Metaclass new'?
		metaclass.superclass(receiver.cls());
		return metaclass;
	}

	public static ProtoObject primitiveRegisterAs(ProtoObject receiver, String name) {
//		System.out.println("primitiveRegisterAs() " + String.valueOf(name) + " " + receiver);
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
		if (symbols.containsKey(value))
			return symbols.get(value);
		ProtoObject symbolClass = receiver.resolveObject("st.redline.Symbol");  // <- should we do primitiveVariableAt so namespaces are used?
		ProtoObject symbol = new ProtoObject(false);
		symbol.cls(symbolClass);
		symbol.javaValue(value);
		symbols.put(value, symbol);
		return symbol;
	}

	public static ProtoObject primitiveNewWith(ProtoObject receiver, Object value) {
		ProtoObject instance = primitiveSend(receiver, "new", null);
		instance.javaValue(value);
		return instance;
	}

	public static ProtoObject primitiveString(ProtoObject receiver, String value) {
		ProtoObject stringClass = receiver.resolveObject("st.redline.String");  // <- should we do primitiveVariableAt so namespaces are used?
		ProtoObject string = new ProtoObject(false);
		string.cls(stringClass);
		string.javaValue(value);
		return string;
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, String selector, ProtoObject classMethodWasFoundIn) {
		// System.out.println("primitiveSend() " + selector);
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, receiver.cls());
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, methodForResult[0]);
		return sendDoesNotUnderstand(receiver, selector, new ProtoObject[] {});
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, ProtoObject arg1, String selector, ProtoObject classMethodWasFoundIn) {
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, receiver.cls(), arg1);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, methodForResult[0], arg1);
		return sendDoesNotUnderstand(receiver, selector, new ProtoObject[] {arg1});
	}

	public static ProtoObject primitiveSend(ProtoObject receiver, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, String selector, ProtoObject classMethodWasFoundIn) {
		ProtoMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5);
		ProtoObject[] methodForResult = {null};
		method = methodFor(receiver.cls().superclass(), selector, methodForResult);
		if (method != null)
			return method.applyTo(receiver, methodForResult[0], arg1, arg2, arg3, arg4, arg5);
		return sendDoesNotUnderstand(receiver, selector, new ProtoObject[] {arg1, arg2, arg3, arg4, arg5});
	}

//	public ProtoObject ping() {
//		// System.out.println("here1 " + this + " " + this.cls() + " Proto  " + classRegistry.get("ProtoObject") + " symbol " + classRegistry.get("Symbol"));
//		return this;
//	}

	private static ProtoObject sendDoesNotUnderstand(ProtoObject receiver, String selector, ProtoObject[] arguments) {
		throw RedlineException.withMessage("TODO -  need to implement send of doesNotUnderstand - '" + selector + "' " + receiver);
	}

	private static ProtoMethod methodFor(ProtoObject object, String selector, ProtoObject[] methodForResult) {
		ProtoMethod method;
		ProtoObject superclass = object;
		while ((method = superclass.methodAt(selector)) == null)
			if ((superclass = superclass.superclass()) == null)
				break;
		methodForResult[0] = superclass;
		return method;
	}

	public static ProtoObject primitiveResolveObject(ProtoObject receiver, String name) {
//		System.out.println("primitiveResolveObject() " + name + " " + receiver);
		ProtoObject object = receiver.resolveObject(name);
		if (object != null)
			return object;
		// TODO.JCL should we return 'nil'?
		throw new IllegalStateException("Class '" + name + "' not found.");
	}

	private ProtoObject resolveObject(String name) {
		if (classRegistry.containsKey(name))
			return classRegistry.get(name);

		if (Character.isUpperCase(name.charAt(0))) {
			String fullyQualifiedName = ProtoObject.primitivePackageAt(this, name);
			if (fullyQualifiedName != null)
				return primitiveResolveObject(this, fullyQualifiedName);
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

	public static String primitivePackageAt(ProtoObject receiver, String name) {
		String fullyQualifiedName = receiver.packageAt(name);
		if (fullyQualifiedName != null)
			return fullyQualifiedName;
		return packageMap.get(name);
	}

	public static void primitivePackageAtPut(ProtoObject receiver, String name, String packageName) {
		receiver.packageAtPut(name, packageName);
	}

	protected ProtoObject loadObject(String name) {
		try {
			return (ProtoObject) Class.forName(name, true, classLoader()).newInstance();
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
	}

	private ClassLoader classLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	private void packageAtPut(String name, String packageName) {
		data.packageAtPut(name, packageName);
	}

	private String packageAt(String name) {
		if (isClass())
			return data.packageAt(name);
		ProtoObject cls = cls();
		if (cls != null)
			return cls.packageAt(name);
		return null;
	}

	public void bootstrap() {
		name("<Bootstrap>");
		new Bootstrapper(this).bootstrap();
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

	public void javaValue(Object value) {
		data.javaValue(value);
	}

	public Object javaValue() {
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

	public static ProtoObject primitive_70(ProtoObject receiver, ProtoObject clsMethodFoundIn, ProtoObject arg1) {
		// System.out.println("primitive_70() " + String.valueOf(receiver) + " " + String.valueOf(clsMethodFoundIn) + " " + String.valueOf(arg1));
		return new ProtoObject(receiver);
	}
}
