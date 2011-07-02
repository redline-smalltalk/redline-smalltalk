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

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

public class ClassObjectData extends InstanceObjectData {

	private final Map<String, RMethod> methodDictionary;
	private final Map<String, RObject> poolDictionary;
	private final Map<String, RObject> instanceVariables;
	private final Map<String, RObject> classInstanceVariables;

	private RObject superclass;
	private boolean bootstrapped;
	private String primitiveName;
	private String primitiveSourceFile;
	private RObject category;

	public ClassObjectData(RObject cls, RObject superclass, Map<String, RObject> variables, Map<String, RMethod> methodDictionary) {
		super(cls, variables);
		this.superclass = superclass;
		this.methodDictionary = methodDictionary;
		this.bootstrapped = false;
		// TODO.JCL - might be better for memory if we make these maps on request.
		this.poolDictionary = new Hashtable<String, RObject>();
		this.instanceVariables = new Hashtable<String, RObject>();
		this.classInstanceVariables = new Hashtable<String, RObject>();
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
		// System.out.println("methodAtPut() " + selector + " " + method.getClass().getName());
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
		// System.out.println("primitiveCategory() " + String.valueOf(category.primitiveValue()));
		this.category = category;
	}

	public void primitiveAddPoolNamed(RObject variable) {
		// System.out.println("primitiveAddPoolNamed() " + String.valueOf(variable.primitiveValue()));
		String name = String.valueOf(variable.primitiveValue());
		if (primitiveHasPoolNamed(name))
			throw new IllegalStateException("Pool named '" + name + "' already defined.");
		poolDictionary.put(name, nil());
	}

	public boolean primitiveHasPoolNamed(String name) {
		return poolDictionary.containsKey(name) || (superclass != null && superclass.primitiveHasPoolNamed(name));
	}

	public void primitiveAddClassInstanceVariableNamed(RObject variable) {
		primitiveAddClassInstanceVariableNamed(String.valueOf(variable.primitiveValue()));
	}

	private void primitiveAddClassInstanceVariableNamed(String name) {
		// System.out.println("primitiveAddClassInstanceVariableNamed() " + name);
	}

	public void primitiveInitializeClassInstanceVariables() {
		// System.out.println("primitiveInitializeClassInstanceVariables()");
		for (String name : primitiveClassInstanceVariableNames())
			primitiveAddClassInstanceVariableNamed(name);
	}

	public boolean primitiveHasInstanceClassVariableNamed(String name) {
		return classInstanceVariables.containsKey(name);
	}

	public List<String> primitiveClassInstanceVariableNames() {
		List<String> names = new ArrayList<String>();
		if (superclass != null)
			for (String name : ((ClassObjectData) superclass.data).classInstanceVariables.keySet())
				names.add(name);
		for (String name : classInstanceVariables.keySet())
			names.add(name);
		return names;
	}

	public void primitiveAddClassVariableNamed(RObject variable) {
		// System.out.println("primitiveAddClassVariableNamed() " + String.valueOf(variable.primitiveValue()));
		String name = String.valueOf(variable.primitiveValue());
		if (primitiveHasClassVariableNamed(name))
			throw new IllegalStateException("Variable named '" + name + "' already defined in class.");
		variables().put(name, nil());
	}

	public boolean primitiveHasClassVariableNamed(String name) {
		return variables().containsKey(name) || (superclass != null && superclass.primitiveHasClassVariableNamed(name));
	}

	public void primitiveAddInstanceVariableNamed(RObject variable) {
		// System.out.println("primitiveAddInstanceVariableNamed() " + String.valueOf(variable.primitiveValue()));
		String name = String.valueOf(variable.primitiveValue());
		if (primitiveHasInstanceVariableNamed(name))
			throw new IllegalStateException("Variable named '" + name + "' already defined.");
		instanceVariables.put(name, nil());
	}

	public boolean primitiveHasInstanceVariableNamed(String name) {
		return instanceVariables.containsKey(name) || (superclass != null && superclass.primitiveHasInstanceVariableNamed(name));
	}

	private RObject nil() {
		return Smalltalk.instance().primitiveAt("nil");
	}
}
