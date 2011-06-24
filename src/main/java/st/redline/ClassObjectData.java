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

public class ClassObjectData extends InstanceObjectData {

	private final Map<String, RMethod> methodDictionary;
	private RObject superclass;
	private boolean bootstrapped;
	private String primitiveName;
	private String primitiveSourceFile;

	public ClassObjectData(RObject cls, RObject superclass, Map<String, RObject> variables, Map<String, RMethod> methodDictionary) {
		super(cls, variables);
		this.superclass = superclass;
		this.methodDictionary = methodDictionary;
		this.bootstrapped = false;
	}

	public RObject superclass() {
		return superclass;
	}

	public Map<String, RMethod> methodDictionary() {
		return methodDictionary;
	}

	public RMethod methodAt(String selector) {
		return methodDictionary.get(selector);
	}

	public void methodAtPut(String selector, RMethod method) {
		System.out.println("methodAtPut() " + selector + " " + method.getClass().getName());
		methodDictionary.put(selector, method);
	}

	public void bootstrapped(boolean bootstrapped) {
		this.bootstrapped = bootstrapped;
	}

	public boolean isBootstrapped() {
		return bootstrapped;
	}

	public boolean isClass() {
		return true;
	}

	public String primitiveName() {
		return primitiveName;
	}

	public void primitiveName(String primitiveName) {
		this.primitiveName = primitiveName;
	}

	public String primitiveSourceFile() {
		return primitiveSourceFile;
	}

	public void primitiveSourceFile(String primitiveSourceFile) {
		this.primitiveSourceFile = primitiveSourceFile;
	}

	public Object primitiveValue() {
		throw new IllegalStateException("A class doesn't have a primitive value.");
	}

	public void primitiveValue(Object primitiveValue) {
		throw new IllegalStateException("A class doesn't have a primitive value.");
	}

	public void primitiveCategory(RObject category) {
		System.out.println("primitiveCategory() " + category);
	}

	public void primitiveAddPoolNamed(RObject variable) {
		System.out.println("primitiveAddPoolNamed() " + variable);
	}

	public void primitiveAddClassInstanceVariableNamed(RObject variable) {
		System.out.println("primitiveAddClassInstanceVariableNamed() " + variable);
	}

	public void primitiveAddClassVariableNamed(RObject variable) {
		System.out.println("primitiveAddClassVariableNamed() " + variable);
	}

	public void primitiveAddInstanceVariableNamed(RObject variable) {
		System.out.println("primitiveAddInstanceVariableNamed() " + variable);
	}
}
