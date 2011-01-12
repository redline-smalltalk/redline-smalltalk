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
public class ProtoObject {

	private static final int CLASS_OFFSET = 0;
	private static final int SUPERCLASS_OFFSET = 1;
	private static final int CLASS_OOP_SIZE = 2;
	private static final int INSTANCE_OOP_SIZE = 1;

	public ProtoObject[] oop;
	public ProtoData data;

	public static ProtoObject classInstance() {
		return new ProtoObject(true);
	}

	public static ProtoObject instanceInstance() {
		return new ProtoObject(false);
	}

	public static Map<String, ProtoMethod> createBasicMethodDictionary() {
		return new Hashtable<String, ProtoMethod>();
	}

	public ProtoObject() {
		this(false);
	}

	public ProtoObject(boolean asClass) {
		if (asClass)
			initializeAsClass();
		else
			initializeAsInstance();
	}

	private void initializeAsClass() {
		oop = new ProtoObject[CLASS_OOP_SIZE];
		data = new ClassProtoData(createBasicMethodDictionary());
	}

	private void initializeAsInstance() {
		oop = new ProtoObject[INSTANCE_OOP_SIZE];
		data = new InstanceProtoData();
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject argument, String selector) {
		return sendDoesNotUnderstand(receiver, selector);
	}

	public static ProtoObject send(ProtoObject receiver, ProtoObject argument1, ProtoObject argument2, String selector) {
		return sendDoesNotUnderstand(receiver, selector);
	}

	public static ProtoObject send(ProtoObject receiver, String selector) {
		ProtoMethod method = receiver.oop[CLASS_OFFSET].data.methodAt(selector);
		if (method != null)
			return method.applyTo(receiver);
		ProtoObject superclass = receiver.oop[CLASS_OFFSET].oop[SUPERCLASS_OFFSET];
		while ((method = superclass.data.methodAt(selector)) == null)
			if ((superclass = superclass.oop[SUPERCLASS_OFFSET]) == null)
				break;
		if (method != null)
			return method.applyTo(receiver);
		return sendDoesNotUnderstand(receiver, selector);
	}

	private static ProtoObject sendDoesNotUnderstand(ProtoObject receiver, String selector) {
		throw new RuntimeException("TODO -  need to implement send of doesNotUnderstand - '" + selector + "'");
	}
}
