/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class ClassInstanceVariableName extends VariableName {

	public ClassInstanceVariableName(String value, int line) {
		super(value, line);
	}

	public boolean isField() {
		return true;
	}

	public boolean isClassInstanceField() {
		return true;
	}
}
