/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;

class Array extends Primary implements ArrayElement {

	private final ArrayList<ArrayElement> elements;

	Array() {
		elements = new ArrayList<ArrayElement>();
	}

	void add(ArrayElement arrayElement) {
		if (arrayElement != null)
			elements.add(arrayElement);
	}

	int size() {
		return elements.size();
	}

	ArrayElement get(int index) {
		return elements.get(index);
	}

	public String value() {
		return "<array>";
	}

	public int line() {
		return elements.get(0).line();
	}

	public void accept(NodeVisitor nodeVisitor) {
	}
}
