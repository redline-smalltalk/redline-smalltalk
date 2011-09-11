/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class VariableName extends BasePrimary {

	protected final String value;
	protected final int line;
	protected int index;
	private boolean onLoadSideOfExpression;

	public VariableName(String value, int line) {
		this.value = value;
		this.line = line;
		this.onLoadSideOfExpression = true;
		this.index = 0;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}

	public void index(int index) {
		this.index = index;
	}

	public boolean isField() {
		return false;
	}

	public boolean isClassField() {
		return false;
	}

	public boolean isClassInstanceField() {
		return false;
	}

	public boolean isClassPoolField() {
		return false;
	}

	public boolean isInstanceField() {
		return false;
	}

	public boolean isClassReference() {
		return isFirstCharacterUppercase();
	}

	public void onStoreSideOfExpression() {
		onLoadSideOfExpression = false;
	}

	private boolean isFirstCharacterUppercase() {
		return java.lang.Character.isUpperCase(value.charAt(0));
	}

	public boolean isOnLoadSideOfExpression() {
		return onLoadSideOfExpression;
	}

	public int line() {
		return line;
	}

	public String value() {
		return value;
	}

	public boolean isOnStoreSideOfExpression() {
		return !onLoadSideOfExpression;
	}
}
