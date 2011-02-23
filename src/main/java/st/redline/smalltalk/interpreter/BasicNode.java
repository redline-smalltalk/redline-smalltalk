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
package st.redline.smalltalk.interpreter;

import java.util.Map;

public abstract class BasicNode implements Node {

	private final Object value;
	private final int line;

	private int index = 0;
	private boolean loadSideOfExpression = false;

	public BasicNode(Object value) {
		this(value, 0);
	}

	public BasicNode(Object value, int line) {
		this.value = value;
		this.line = line;
	}

	public Object value() {
		return value;
	}

	public int line() {
		return line;
	}

	public void add(Node node) {
		throw new IllegalStateException("Basic nodes don't support node lists.");
	}

	public String toString() {
		return value.toString();
	}

	public boolean isOnLoadSideOfExpression() {
		return loadSideOfExpression;
	}

	public int index() {
		return index;
	}

	public void index(int index) {
		this.index = index;
	}

	protected void registerIn(String name, Map<String, BasicNode> variableAndTemporaryRegistry) {
		if (variableAndTemporaryRegistry.containsKey(name))
			throw new IllegalStateException("Variable '" + name + "' already defined.");
		variableAndTemporaryRegistry.put(name, this);
	}
}
