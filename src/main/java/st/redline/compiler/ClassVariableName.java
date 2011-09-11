/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class ClassVariableName extends VariableName {

	public ClassVariableName(String value, int line) {
		super(value, line);
	}

	public boolean isField() {
		return true;
	}

	public boolean isClassField() {
		return true;
	}
}
