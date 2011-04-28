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
package st.redline.smalltalk.interpreter;

import java.util.HashMap;
import java.util.Map;

public class VariableRegistry {

	private final VariableRegistry parentVariableRegistry;
	private final Map<String,VariableName> variableRegistry;

	public VariableRegistry() {
		this(null);
	}

	public VariableRegistry(VariableRegistry parentVariableRegistry) {
		this(parentVariableRegistry, new HashMap<String, VariableName>());
	}

	public VariableRegistry(VariableRegistry parentVariableRegistry, Map<String, VariableName> variableRegistry) {
		this.parentVariableRegistry = parentVariableRegistry;
		this.variableRegistry = variableRegistry;
	}

	public VariableName get(String key) {
		VariableName variableName = variableRegistry.get(key);
		if (variableName != null)
			return variableName;
		if (parentVariableRegistry != null)
			return parentVariableRegistry.get(key);
		return null;
	}

	public boolean containsKey(String key) {
		if (variableRegistry.containsKey(key))
			return true;
		if (parentVariableRegistry != null)
			return parentVariableRegistry.containsKey(key);
		return false;
	}

	public void put(String key, VariableName variableName) {
		variableRegistry.put(key, variableName);
	}
}
