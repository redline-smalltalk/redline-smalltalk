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
package st.redline.smalltalk;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Hashtable;
import java.util.Map;

/**
 * Provides base for all Smalltalk Objects.
 */
public class RObject {

	private final static Logger log = LoggerFactory.getLogger(RObject.class);

	protected enum Datum { CLASS, INSTANCE, PRIMITIVEINSTANCE }

	public static final int CLASS_OFFSET = 0;
	public static final int SUPERCLASS_OFFSET = 1;
	public static final int CLASS_OOP_SIZE = 2;
	public static final int INSTANCE_OOP_SIZE = 1;

	public RObject[] oop;
	public RData data;

	public static RObject classInstance() {
		return new RObject(Datum.CLASS);
	}

	public static RObject instanceInstance() {
		return new RObject(Datum.INSTANCE);
	}

	public static RObject primitiveInstance() {
		return new RObject(Datum.PRIMITIVEINSTANCE);
	}

	public static Map<String, RMethod> createBasicMethodDictionary() {
		return new Hashtable<String, RMethod>();
	}

	public RObject() {
		this(Datum.INSTANCE);
	}

	public RObject(Datum datum) {
		if (datum == Datum.CLASS)
			initializeAsClass();
		else if (datum == Datum.PRIMITIVEINSTANCE)
			initializeAsPrimitiveInstance();
		else
			initializeAsInstance();
	}

	private void initializeAsClass() {
		oop = new RObject[CLASS_OOP_SIZE];
		data = new ClassData(createBasicMethodDictionary(), this);
	}

	private void initializeAsInstance() {
		oop = new RObject[INSTANCE_OOP_SIZE];
		data = new InstanceData(this);
	}

	private void initializeAsPrimitiveInstance() {
		oop = new RObject[INSTANCE_OOP_SIZE];
		data = new PrimitiveInstanceData(this);
	}

	public String toString() {
		return name();
	}

	protected String name() {
		String name = oop[CLASS_OFFSET].data != null ? oop[CLASS_OFFSET].data.primitiveName() : null;
		if (name != null)
			return name;
		return data.primitiveName();
	}

	public Smalltalk smalltalk() {
		return (Smalltalk) getClass().getClassLoader();
	}

	public static RBlock createBlock(String blockClassName) {
		Smalltalk smalltalk = Smalltalk.instance();
		if (smalltalk.verboseOn())
			log.info("creating instance " + blockClassName);
		try {
			return (RBlock) smalltalk.loadClass(blockClassName).newInstance();
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}

	public static void bindMethod(String className, String methodName, String methodClassName, boolean classMethod) {
		Smalltalk smalltalk = Smalltalk.instance();
		if (smalltalk.verboseOn())
			log.info("binding " + (classMethod ? "class" : "instance") + " method '" + methodClassName + "' as '" + methodName + "' in " + className);
		RObject aClass = smalltalk.cachedObject0(className);
		if (aClass == null)
			throw new IllegalStateException("Can't find class '" + className + "'. Did you name your class the same as the file?");
		RMethod method = tryInstantiateMethod(smalltalk, methodClassName);
		RData binding = classMethod ? aClass.oop[CLASS_OFFSET].data : aClass.data;
		binding.methodAtPut(methodName, method);
	}

	private static RMethod tryInstantiateMethod(Smalltalk classLoader, String methodClassName) {
		RMethod method;
		try {
			System.out.println("tryInstantiateMethod() " + methodClassName);
			method = (RMethod) classLoader.loadClass(methodClassName).newInstance();
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
		return method;
	}

	private static RMethod methodFor(RObject rObject, String selector, RObject[] classMethodFoundIn) {
		RMethod method;
		RObject superclass = rObject;
		while ((method = superclass.data.methodAt(selector)) == null)
			if ((superclass = superclass.oop[SUPERCLASS_OFFSET]) == null)
				break;
		classMethodFoundIn[0] = superclass;
		return method;
	}

	private static RObject primitive_1(RObject receiver, RObject classMethodWasFoundIn, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, RObject arg10) {
		throw new RuntimeException("TODO -  need to implement primitives!");
	}

	private static RObject sendDoesNotUnderstand(RObject receiver, String selector, RObject[] arguments) {
		throw new RuntimeException("TODO -  need to implement send of doesNotUnderstand - '" + selector + "'");
	}

	private static boolean isBootstrapped(RObject object) {
		return (object != null && (object.data.isBootstrapped() || object.oop[CLASS_OFFSET].data.isBootstrapped()));
	}

	private static boolean notBootstrapped(RObject object) {
		return !isBootstrapped(object);
	}

	private static boolean resolveClassObject(RObject object) {
		return resolveClassObject(object, null);
	}

	private static boolean resolveClassObject(RObject object, RObject other) {
		if (!resolveObjectSuperclass(object, other))
			return false;
		if (notBootstrapped(object))
			return true;
		String className = object.name();
		Smalltalk smalltalk = Smalltalk.instance();
		if (smalltalk.verboseOn())
			log.info("Resolving bootstrapped class '" + className + "'");
		return smalltalk.resolveClassObject(className);
	}

	private static boolean resolveObjectSuperclass(RObject object, RObject other) {
		if (object == null)
			return true;
		if (!object.data.isClass())
			return resolveObjectSuperclass(object.oop[CLASS_OFFSET], other);
		RObject superclass = object.oop[SUPERCLASS_OFFSET];
		if (superclass != other)
			return resolveClassObject(superclass, other);
		RObject metaclass = object.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET];
		return resolveClassObject(metaclass, metaclass);
	}

	//
	// send(r,s, c) to send(r,a,a,a,a,a,a,a,a,a,a,s,c)
	// Note: Selector (s) is second last argument. Class method was found is last argument.
	//

	public static RObject send(RObject receiver, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, receiver.oop[CLASS_OFFSET]);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyTo(receiver, newClassMethodWasFoundIn[0]);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, null);
	}

	public static RObject send(RObject receiver, RObject arg, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4, arg5);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, String selector, RObject currentClassMethodWasFoundIn) {
		System.out.println("*** SEND *** " + currentClassMethodWasFoundIn);
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4, arg5, arg6);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, RObject arg10, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.oop[CLASS_OFFSET], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10});
	}
}
