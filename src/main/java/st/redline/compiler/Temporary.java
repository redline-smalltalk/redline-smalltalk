/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Temporary extends VariableName {

	public Temporary(VariableName variableName) {
		super(variableName.value, variableName.line);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, index, value, line);
	}

	public void index(int index) {
		this.index = index;
	}

	public boolean isClassReference() {
		return false;
	}
}
