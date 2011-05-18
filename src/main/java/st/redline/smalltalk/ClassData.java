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

import java.util.Hashtable;
import java.util.Map;

public class ClassData extends RData {

	private final Map<String, RMethod> methodDictionary;

	private String name;
	private boolean bootstrapped;
	private RObject category;
	private Map<String, RObject> instanceVariables;
	private Map<String, RObject> poolDictionaries;

	public ClassData(Map<String, RMethod> methodDictionary, RObject container) {
		super(container);
		this.methodDictionary = methodDictionary;
	}

	public boolean isClass() {
		return true;
	}

	public boolean isBootstrapped() {
		return bootstrapped;
	}

	public void bootstrapped(boolean bootstrapped) {
		this.bootstrapped = bootstrapped;
	}

	public String primitiveName() {
		return name;
	}

	public void primitiveName(String name) {
		this.name = name;
	}

	public boolean hasInstanceVariableNamed(String key) {
		return instanceVariables != null && instanceVariables.containsKey(key);
	}

	public boolean hasClassVariableNamed(String key) {
		return variables != null && variables.containsKey(key);
	}

	public boolean hasClassInstanceVariableNamed(String key) {
		return this.containingObject.oop[RObject.CLASS_OFFSET].data.hasInstanceVariableNamed(key);
	}

	public boolean hasPoolDictionaryNamed(String key) {
		return poolDictionaries != null && poolDictionaries.containsKey(key);
	}

	public void primitiveAddInstanceVariableNamed(RObject variable) {
		String key = variable.data.primitiveValue().toString();
		RObject superclass = this.containingObject.oop[RObject.SUPERCLASS_OFFSET];
		while (superclass != null) {
			if (superclass.data.hasInstanceVariableNamed(key))
				throw new IllegalStateException("Superclass " + superclass + " already defined instance variable '" + key + "'.");
			superclass = superclass.oop[RObject.SUPERCLASS_OFFSET];
		}
		// instance variable names are kept on the class and used when an instance is created.
		if (instanceVariables == null)
			instanceVariables = new Hashtable<String, RObject>();
		instanceVariables.put(key, variable);
	}

	public void primitiveAddClassVariableNamed(RObject variable) {
		String key = variable.data.primitiveValue().toString();
		RObject superclass = this.containingObject.oop[RObject.SUPERCLASS_OFFSET];
		while (superclass != null) {
			if (superclass.data.hasClassVariableNamed(key))
				throw new IllegalStateException("Superclass " + superclass + " already defined class variable '" + key + "'.");
			superclass = superclass.oop[RObject.SUPERCLASS_OFFSET];
		}
		if (variables == null)
			variables = new Hashtable<String, RObject>();
		variables.put(key, variable);
	}

	public void primitiveAddClassInstanceVariableNamed(RObject variable) {
		String key = variable.data.primitiveValue().toString();
		RObject superclass = this.containingObject.oop[RObject.SUPERCLASS_OFFSET];
		if (superclass != null && superclass.data.hasClassInstanceVariableNamed(key))
				throw new IllegalStateException("Superclass " + superclass + " already defined class instance variable '" + key + "'.");
		// class instance variables are variables on the metaclass instance.
		if (this.containingObject.oop[RObject.CLASS_OFFSET].data.variables == null)
			this.containingObject.oop[RObject.CLASS_OFFSET].data.variables = new Hashtable<String, RObject>();
		RObject nil = Smalltalk.instance().nilInstance();
		// copy across any class instance variables in superclass. Each class gets it's own instance.
		if (superclass.oop[RObject.CLASS_OFFSET].data.variables != null)
			for (String string : superclass.oop[RObject.CLASS_OFFSET].data.variables.keySet())
				this.containingObject.oop[RObject.CLASS_OFFSET].data.variables.put(key, nil);
		this.containingObject.oop[RObject.CLASS_OFFSET].data.variables.put(key, variable);
	}

	public void primitiveAddPoolNamed(RObject pool) {
		String key = pool.data.primitiveValue().toString();
		RObject superclass = this.containingObject.oop[RObject.SUPERCLASS_OFFSET];
		while (superclass != null) {
			if (superclass.data.hasPoolDictionaryNamed(key))
				throw new IllegalStateException("Superclass " + superclass + " already defined pool variable '" + key + "'.");
			superclass = superclass.oop[RObject.SUPERCLASS_OFFSET];
		}
		if (poolDictionaries == null)
			poolDictionaries = new Hashtable<String, RObject>();
		poolDictionaries.put(key, pool);
	}

	public void primitiveCategory(RObject category) {
		this.category = category;
	}

	public Object primitiveValue() {
		throw classesDontHavePrimitiveValues();
	}

	public void primitiveValue(Object value) {
		throw classesDontHavePrimitiveValues();
	}

	public RMethod methodAt(String selector) {
		return methodDictionary.get(selector);
	}

	public void methodAtPut(String selector, RMethod method) {
		methodDictionary.put(selector, method);
	}

	private IllegalStateException classesDontHavePrimitiveValues() {
		return new IllegalStateException("Class objects don't have primitive values.");
	}
}
