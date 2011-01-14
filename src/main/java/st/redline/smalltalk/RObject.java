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
*/
package st.redline.smalltalk;

import java.util.Hashtable;
import java.util.Map;

/**
 * Provides base for all Smalltalk Objects.
 */
public class RObject {

	protected enum Datum { CLASS, INSTANCE, PRIMITIVEINSTANCE };

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
		data = new ClassData(createBasicMethodDictionary());
	}

	private void initializeAsInstance() {
		oop = new RObject[INSTANCE_OOP_SIZE];
		data = new InstanceData();
	}

	private void initializeAsPrimitiveInstance() {
		oop = new RObject[INSTANCE_OOP_SIZE];
		data = new PrimitiveInstanceData();
	}

	private static RMethod methodFor(RObject rObject, String selector) {
		RMethod method;
		RObject superclass = rObject;
		while ((method = superclass.data.methodAt(selector)) == null)
			if ((superclass = superclass.oop[SUPERCLASS_OFFSET]) == null)
				break;
		return method;
	}

	private static RObject sendDoesNotUnderstand(RObject receiver, String selector, RObject[] arguments) {
		throw new RuntimeException("TODO -  need to implement send of doesNotUnderstand - '" + selector + "'");
	}

	//
	// send(r,s) to send(r,a,a,a,a,a,a,a,a,a,a,s)
	// Note: Selector (s) is last argument.
	//

	public static RObject send(RObject receiver, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyTo(receiver);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyTo(receiver);
		return sendDoesNotUnderstand(receiver, selector, null);
	}

	public static RObject send(RObject receiver, RObject arg, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9});
	}

	public static RObject send(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5, RObject arg6, RObject arg7, RObject arg8, RObject arg9, RObject arg10, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {arg1,  arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10});
	}
}
