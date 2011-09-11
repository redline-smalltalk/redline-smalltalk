/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BinarySelectorMessagePattern implements MessagePattern {

	private final BinarySelector binarySelector;
	private final VariableName variableName;

	public BinarySelectorMessagePattern(BinarySelector binarySelector, VariableName variableName) {
		this.binarySelector = binarySelector;
		this.variableName = variableName;
		variableName.index(1);
	}

	public int line() {
		return binarySelector.line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, binarySelector.value, binarySelector.line, variableName);
	}
}
