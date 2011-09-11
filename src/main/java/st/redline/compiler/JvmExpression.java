/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public abstract class JvmExpression implements Expression {

	protected boolean resultLeftOnStack;
	protected boolean duplicateResultOnStack;

	public JvmExpression() {
		resultLeftOnStack = true;
		duplicateResultOnStack = false;
	}

	public boolean isResultLeftOnStack() {
		return resultLeftOnStack;
	}

	public void leaveResultOnStack() {
		resultLeftOnStack = true;
	}

	public boolean isResultDuplicatedOnStack() {
		return duplicateResultOnStack;
	}

	public void duplicateResultOnStack() {
		duplicateResultOnStack = true;
	}
}
