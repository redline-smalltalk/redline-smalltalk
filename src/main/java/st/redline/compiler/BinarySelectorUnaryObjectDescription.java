/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BinarySelectorUnaryObjectDescription implements VisitableNode {

	private final BinarySelector binarySelector;
	private final UnaryObjectDescription unaryObjectDescription;

	public BinarySelectorUnaryObjectDescription(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		this.binarySelector = binarySelector;
		this.unaryObjectDescription = unaryObjectDescription;
	}

	public void accept(NodeVisitor nodeVisitor) {
		unaryObjectDescription.accept(nodeVisitor);
		nodeVisitor.visit(binarySelector, binarySelector.value, binarySelector.line);
	}
}
