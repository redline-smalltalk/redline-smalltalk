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

import java.util.List;
import java.util.Map;

public class InstanceObjectData implements ObjectData {

	private final Map<String, RObject> variables;
	private RObject cls;
	private Object primitiveValue;

	public InstanceObjectData(RObject cls, Map<String, RObject> variables) {
		this.cls = cls;
		this.variables = variables;
	}

	public RObject cls() {
		return cls;
	}

	public void cls(RObject cls) {
		this.cls = cls;
	}

	public RObject superclass() {
		throw new IllegalStateException("Can't access superclass from instance.");
	}

	public Map<String, RObject> variables() {
		return variables;
	}

	public Map<String, RMethod> methodDictionary() {
		throw new IllegalStateException("Can't access method dictionary from instance.");
	}

	public RMethod methodAt(String selector) {
		throw new IllegalStateException("Can't access methods from instance.");
	}

	public void methodAtPut(String selector, RMethod method) {
		throw new IllegalStateException("Can't add methods to an instance.");
	}

	public void bootstrapped(boolean bootstrapped) {
		throw new IllegalStateException("Can't bootstrap an instance.");
	}

	public boolean isBootstrapped() {
		return false;
	}

	public boolean isClass() {
		return false;
	}

	public String primitiveName() {
		throw new IllegalStateException("A primitive instance has no name.");
	}

	public void primitiveName(String primitiveName) {
		throw new IllegalStateException("A primitive instance has no name.");
	}

	public String primitiveSourceFile() {
		throw new IllegalStateException("Can't get source file from an instance.");
	}

	public void primitiveSourceFile(String primitiveSourceFile) {
		throw new IllegalStateException("Can't name source file on an instance.");
	}

	public Object primitiveValue() {
		return primitiveValue;
	}

	public void primitiveValue(Object primitiveValue) {
		this.primitiveValue = primitiveValue;
	}

	public void primitiveCategory(RObject category) {
		throw new IllegalStateException("Can't categorise an instance.");
	}

	public void primitiveAddPoolNamed(RObject variable) {
		throw new IllegalStateException("Can't add a pool to an instance.");
	}

	public boolean primitiveHasPoolNamed(String name) {
		throw new IllegalStateException("Can't query a pool on an instance.");
	}

	public void primitiveAddClassInstanceVariableNamed(RObject variable) {
		throw new IllegalStateException("Can't add a class instance variable to an instance.");
	}

	public void primitiveInitializeClassInstanceVariables() {
		throw new IllegalStateException("Can't initialize class instance variables on an instance.");
	}

	public List<String> primitiveClassInstanceVariableNames() {
		throw new IllegalStateException("Can't query class instance variables on an instance.");
	}

	public boolean primitiveHasClassInstanceVariableNamed(String name) {
		throw new IllegalStateException("Can't query a class instance variable on an instance.");
	}

	public void primitiveAddClassVariableNamed(RObject variable) {
		throw new IllegalStateException("Can't add a class variable to an instance.");
	}

	public boolean primitiveHasClassVariableNamed(String name) {
		throw new IllegalStateException("Can't query a class variable on an instance.");
	}

	public void primitiveAddInstanceVariableNamed(RObject variable) {
		throw new IllegalStateException("Can't add an instance variable to an instance.");
	}

	public boolean primitiveHasInstanceVariableNamed(String name) {
		throw new IllegalStateException("Can't query an instance variable on an instance.");
	}
}
