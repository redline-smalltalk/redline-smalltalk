/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class Number extends Primary implements ArrayElement {

	private final int line;
	private final String value;
	private int index;
	private boolean insideArray = false;

	Number(String d1, String r, String m1, String d2, int line, String d3, String e, String m2, String d4) {
		this.line = line;
		StringBuffer number = new StringBuffer();
		if (d1 != null)
			number.append(d1);
		if (r != null)
			number.append(r);
		if (m1 != null)
			number.append(m1);
		if (d2 != null)
			number.append(d2);
		if (d3 != null) {
			number.append(".");
			number.append(d3);
		}
		if (e != null)
			number.append(e);
		if (m2 != null)
			number.append(m2);
		if (d4 != null)
			number.append(d4);
		value = number.toString();
	}

	public void insideArray() {
		insideArray = true;
	}

	public boolean isInsideArray() {
		return insideArray;
	}

	public int index() {
		return index;
	}

	public void index(int index) {
		this.index = index;
	}

	public String value() {
		return value;
	}

	public int line() {
		return line;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, value(), line());
	}
}
