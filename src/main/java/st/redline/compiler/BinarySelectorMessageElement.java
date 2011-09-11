/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BinarySelectorMessageElement implements MessageElement {

	private final BinarySelector binarySelector;
	private final UnaryObjectDescription unaryObjectDescription;

	public BinarySelectorMessageElement(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		this.binarySelector = binarySelector;
		this.unaryObjectDescription = unaryObjectDescription;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, binarySelector.value, binarySelector.line, unaryObjectDescription);
	}
}
