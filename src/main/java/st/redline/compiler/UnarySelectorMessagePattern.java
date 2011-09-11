/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class UnarySelectorMessagePattern implements MessagePattern {

	private final UnarySelector unarySelector;

	public UnarySelectorMessagePattern(UnarySelector unarySelector) {
		this.unarySelector = unarySelector;
	}

	public int line() {
		return unarySelector.line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, unarySelector.value, unarySelector.line);
	}
}
