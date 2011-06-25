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

import java.util.Map;

public class RObject implements ObjectData {

	private final ObjectData data;

	public RObject() {
		this(null);
	}

	public RObject(ObjectData data) {
		this.data = data;
	}

	public Smalltalk smalltalk() {
		return Smalltalk.instance();
	}

	public static RObject createClassWith(RObject cls, RObject superclass) {
		return new RClass(cls, superclass);
	}

	public static RObject createInstanceWith(RObject cls) {
		return new RInstance(cls);
	}

	public RObject cls() {
		return data.cls();
	}

	public void cls(RObject cls) {
		data.cls(cls);
	}

	public RObject superclass() {
		return data.superclass();
	}

	public Map<String, RObject> variables() {
		return data.variables();
	}

	public Map<String, RMethod> methodDictionary() {
		return data.methodDictionary();
	}

	public RMethod methodAt(String selector) {
		return data.methodAt(selector);
	}

	public void methodAtPut(String selector, RMethod method) {
		data.methodAtPut(selector, method);
	}

	public boolean isBootstrapped() {
		return data.isBootstrapped();
	}

	public void bootstrapped(boolean bootstrapped) {
		data.bootstrapped(bootstrapped);
	}

	public boolean isClass() {
		return data.isClass();
	}

	public String primitiveName() {
		return data.primitiveName();
	}

	public void primitiveName(String primitiveName) {
		data.primitiveName(primitiveName);
	}

	public String primitiveSourceFile() {
		return data.primitiveSourceFile();
	}

	public void primitiveSourceFile(String primitiveSourceFile) {
		System.out.println("primitiveSourceFile() " + primitiveSourceFile);
		data.primitiveSourceFile(primitiveSourceFile);
	}

	public Object primitiveValue() {
		return data.primitiveValue();
	}

	public void primitiveValue(Object primitiveValue) {
		data.primitiveValue(primitiveValue);
	}

	public void primitiveCategory(RObject category) {
		data.primitiveCategory(category);
	}

	public void primitiveAddPoolNamed(RObject variable) {
		data.primitiveAddPoolNamed(variable);
	}

	public boolean primitiveHasPoolNamed(String name) {
		return data.primitiveHasPoolNamed(name);
	}

	public void primitiveAddClassInstanceVariableNamed(RObject variable) {
		data.primitiveAddClassInstanceVariableNamed(variable);
	}

	public void primitiveAddClassVariableNamed(RObject variable) {
		data.primitiveAddClassVariableNamed(variable);
	}

	public void primitiveAddInstanceVariableNamed(RObject variable) {
		data.primitiveAddInstanceVariableNamed(variable);
	}

	public String toString() {
		return name();
	}

	public String name() {
		String name = cls() != null ? cls().primitiveName() : null;
		if (name != null)
			return name;
		return data.primitiveName();
	}

	private static RObject sendDoesNotUnderstand(RObject receiver, String selector, RObject[] arguments) {
		throw new RuntimeException("TODO -  need to implement send of doesNotUnderstand - '" + selector + "'");
	}

	private static boolean resolveClassObject(RObject object) {
		return resolveClassObject(object, null);
	}

	private static boolean isBootstrapped(RObject object) {
		return (object != null && (object.isBootstrapped() || object.cls().isBootstrapped()));
	}

	private static boolean isNotBootstrapped(RObject object) {
		return !isBootstrapped(object);
	}

	private static boolean resolveClassObject(RObject object, RObject other) {
		System.out.println("resolveClassObject() " + object + " " + other);
		if (!resolveObjectSuperclass(object, other))
			return false;
		if (isNotBootstrapped(object))
			return true;
		String className = object.name();
		return Smalltalk.instance().resolveClassObject(className);
	}

	private static boolean resolveObjectSuperclass(RObject object, RObject other) {
		System.out.println("resolveClassObjectSuperclass() " + object + " " + other);
		if (object == null)
			return true;
		if (!object.isClass())
			return resolveObjectSuperclass(object.cls(), other);
		RObject superclass = object.superclass();
		if (superclass != other)
			return resolveClassObject(superclass, other);
		RObject metaclass = object.cls().superclass();
		return resolveClassObject(metaclass, metaclass);
	}

	private static RMethod methodFor(RObject rObject, String selector, RObject[] classMethodFoundIn) {
		RMethod method;
		RObject superclass = rObject;
		while ((method = superclass.methodAt(selector)) == null)
			if ((superclass = superclass.superclass()) == null)
				break;
		classMethodFoundIn[0] = superclass;
		return method;
	}

	//
	// primitives (invoked when <primitive: x> found in source.>
	//

	private static RObject primitive_1(RObject receiver, RObject classMethodWasFoundIn, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, RObject arg10) {
		throw new RuntimeException("TODO -  need to implement primitives!");
	}

	//
	// send(r,s, c) to send(r,a,a,a,a,a,a,a,a,a,a,s,c)
	// Note: Selector (s) is second last argument. Class method was found is last argument.
	//

	public static RObject send(RObject receiver, String selector, RObject currentClassMethodWasFoundIn) {
		System.out.println("send " + selector + " " + currentClassMethodWasFoundIn);
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, receiver.cls());
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyTo(receiver, newClassMethodWasFoundIn[0]);
		if (receiver.isBootstrapped())
			if (resolveClassObject(receiver))
				return send(receiver, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, null);
	}

	public static RObject superSend(RObject receiver, String selector, RObject currentClassMethodWasFoundIn) {
		System.out.println("superSend " + selector + " " + currentClassMethodWasFoundIn);
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyTo(receiver, currentClassMethodWasFoundIn.superclass());
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyTo(receiver, newClassMethodWasFoundIn[0]);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, null);
	}

	public static RObject send(RObject receiver, RObject arg, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg});
	}

	public static RObject superSend(RObject receiver, RObject arg, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, arg5, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, String selector, RObject currentClassMethodWasFoundIn) {
		System.out.println("*** SEND *** " + currentClassMethodWasFoundIn);
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, String selector, RObject currentClassMethodWasFoundIn) {
		System.out.println("*** SUPERSEND *** " + currentClassMethodWasFoundIn);
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, arg5, arg6, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, RObject arg10, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = receiver.cls().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, receiver.cls(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(receiver.cls().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return send(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10});
	}

	public static RObject superSend(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, RObject arg10, String selector, RObject currentClassMethodWasFoundIn) {
		RMethod method = currentClassMethodWasFoundIn.superclass().methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, currentClassMethodWasFoundIn.superclass(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		RObject[] newClassMethodWasFoundIn = {null};
		method = methodFor(currentClassMethodWasFoundIn.superclass().superclass(), selector, newClassMethodWasFoundIn);
		if (method != null)
			return method.applyToWith(receiver, newClassMethodWasFoundIn[0], arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		if (isBootstrapped(receiver))
			if (resolveClassObject(receiver))
				return superSend(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, selector, currentClassMethodWasFoundIn);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10});
	}
}
