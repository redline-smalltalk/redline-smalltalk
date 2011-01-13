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

	public static final int CLASS_OFFSET = 0;
	public static final int SUPERCLASS_OFFSET = 1;
	public static final int CLASS_OOP_SIZE = 2;
	public static final int INSTANCE_OOP_SIZE = 1;

	public RObject[] oop;
	public RData data;

	public static RObject classInstance() {
		return new RObject(true);
	}

	public static RObject instanceInstance() {
		return new RObject(false);
	}

	public static Map<String, RMethod> createBasicMethodDictionary() {
		return new Hashtable<String, RMethod>();
	}

	public RObject() {
		this(false);
	}

	public RObject(boolean asClass) {
		if (asClass)
			initializeAsClass();
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

	public static RObject send(RObject receiver, RObject argument, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, argument);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, argument);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {argument});
	}

	public static RObject send(RObject receiver, RObject argument1, RObject argument2, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyToWith(receiver, argument1, argument2);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyToWith(receiver, argument1, argument2);
		return sendDoesNotUnderstand(receiver, selector, new RObject[] {argument1,  argument1});
	}

	public static RObject send(RObject receiver, String selector) {
		RMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyTo(receiver);
		method = methodFor(receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET], selector);
		if (method != null)
			return method.applyTo(receiver);
		return sendDoesNotUnderstand(receiver, selector, null);
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
}
