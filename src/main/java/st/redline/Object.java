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

import java.util.HashMap;
import java.util.Map;

public class Object {

	private final Map<String, Object> classes;

	public Object() {
		classes = new HashMap<String, Object>();
	}

	public Object primitiveRegisterAs(String name) {
		classes.put(name, this);
		return this;
	}

	public void primitiveMain(String[] args) {
	}

	public Object primitiveVariableAt(String name) {
		if (primitiveIsInstanceVariable(name))
			throw new IllegalStateException("todo - implement");
		if (primitiveIsClassVariable(name))
			throw new IllegalStateException("todo - implement");
		return primitiveResolveObject(name);
	}

	public boolean primitiveIsInstanceVariable(String name) {
		return false;
	}

	public boolean primitiveIsClassVariable(String name) {
		return false;
	}

	public Object primitiveResolveObject(String name) {
		Object object = resolveObject(name);
		if (object != null)
			return object;
		// TODO.JCL search through namespaces to find object.
		return resolveObject("st.redline." + name);
	}

	private Object resolveObject(String name) {
		// It is expected the loading of an object results in it registering a Smalltalk class in the classes map.
		if (classes.containsKey(name))
			return classes.get(name);
		if (loadObject(name))
			return classes.get(name);
		// TODO.JCL should we return 'nil'?
		return null;
	}

	private boolean loadObject(String name) {
		try {
			return Class.forName(name, true, classLoader()).newInstance() != null;
		} catch (Exception e) {
			return false;
		}
	}

	private ClassLoader classLoader() {
		return Thread.currentThread().getContextClassLoader();
	}
}
