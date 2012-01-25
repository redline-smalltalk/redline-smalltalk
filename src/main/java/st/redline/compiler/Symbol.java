/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class Symbol extends ValuePrimary implements ArrayElement {

	private int index;

	Symbol() {
		super(null, 0);
	}

	public int index() {
		return index;
	}

	public void index(int index) {
		this.index = index;
	}

	void valueAndLine(String value, int line) {
		value(value);
		line(line);
	}

	void addValueAndLine(String value, int line) {
		if (line() == 0)
			line(line);
		if (value() == null)
			value(value);
		else
			value(value() + value);
	}

	public void accept(NodeVisitor nodeVisitor) {
	}
}
