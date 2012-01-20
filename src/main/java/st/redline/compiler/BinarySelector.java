/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class BinarySelector extends ValuePrimary {

	BinarySelector() {
		super("", 0);
	}

	void add(String value, int line) {
		line(line);
		value(value() + value);
	}
}
