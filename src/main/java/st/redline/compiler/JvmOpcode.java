/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JvmOpcode {

	private final int value;
	private final int line;

	public JvmOpcode(int value, int line) {
		this.value = value;
		this.line = line;
	}

	public int value() {
		return value;
	}

	public int line() {
		return line;
	}
}
