/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class UnarySelectorMessageElement implements MessageElement {

	private final UnarySelector unarySelector;

	UnarySelectorMessageElement(UnarySelector unarySelector) {
		this.unarySelector = unarySelector;
	}

	UnarySelector unarySelector() {
		return unarySelector;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this, unarySelector.value(), unarySelector.line());
	}
}
