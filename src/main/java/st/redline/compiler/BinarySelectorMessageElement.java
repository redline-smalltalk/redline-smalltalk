/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BinarySelectorMessageElement implements MessageElement {

	private final BinarySelector binarySelector;
	private final UnaryObjectDescription unaryObjectDescription;

	BinarySelectorMessageElement(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		this.binarySelector = binarySelector;
		this.unaryObjectDescription = unaryObjectDescription;
	}

	BinarySelector binarySelector() {
		return binarySelector;
	}

	UnaryObjectDescription unaryObjectDescription() {
		return unaryObjectDescription;
	}
}
