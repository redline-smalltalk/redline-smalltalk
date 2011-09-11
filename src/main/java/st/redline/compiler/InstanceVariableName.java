/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class InstanceVariableName extends VariableName {

	public InstanceVariableName(String value, int line) {
		super(value, line);
	}

	public boolean isField() {
		return true;
	}

	public boolean isInstanceField() {
		return true;
	}
}
