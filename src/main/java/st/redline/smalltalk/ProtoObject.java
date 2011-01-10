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

	private static final int BASIC_SIZE = 2;
	private static final int CLASS_OFFSET = 0;
	private static final int SUPERCLASS_OFFSET = 1;

	public ProtoObject[] variables;
	public Map<String, ProtoMethod> methodDictionary;

	public static ProtoObject createBasicClass(int size) {
		ProtoObject classObject = new ProtoObject(size + BASIC_SIZE);
		classObject.methodDictionary = createBasicMethodDictionary();
		return classObject;
	}

	public static Map<String, ProtoMethod> createBasicMethodDictionary() {
		return new Hashtable<String, ProtoMethod>();
	}

	public ProtoObject() {
		this(1);
	}

	public ProtoObject(int basicSize) {
		createVariables(basicSize);
	}

	private void createVariables(int basicSize) {
		variables = new ProtoObject[basicSize];
	}

	public static ProtoObject send(ProtoObject receiver, String unarySelector) {
		ProtoMethod method = receiver.basicClass().basicMethodDictionaryAt(unarySelector);
		if (method != null)
			return method.applyTo(receiver);
		ProtoObject superclass = receiver.basicClass().basicSuperclass();
		while ((method = superclass.basicMethodDictionaryAt(unarySelector)) == null)
			if ((superclass = superclass.basicSuperclass()) == null)
				break;
		if (method != null)
			return method.applyTo(receiver);
		return sendDoesNotUnderstand(receiver, unarySelector);
	}

	private static ProtoObject sendDoesNotUnderstand(ProtoObject receiver, String unarySelector) {
		throw new RuntimeException("TODO -  need to implement send of doesNotUnderstand");
	}

	public ProtoObject basicClass() {
		return variables[CLASS_OFFSET];
	}

	public void basicClass(ProtoObject classObject) {
		variables[CLASS_OFFSET] = classObject;
	}

	public ProtoMethod basicMethodDictionaryAt(String selector) {
		return methodDictionary.get(selector);
	}

	public ProtoObject basicSuperclass() {
		return variables[SUPERCLASS_OFFSET];
	}

	public void basicSuperclass(ProtoObject superclass) {
		variables[SUPERCLASS_OFFSET] = superclass;
	}
}
