/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class UnarySelectorMessageElement implements MessageElement {

	private final UnarySelector unarySelector;

	public UnarySelectorMessageElement(UnarySelector unarySelector) {
		this.unarySelector = unarySelector;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, unarySelector.value, unarySelector.line);
	}
}
