/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class BinaryObjectDescription implements VisitableNode {

	private final Primary primary;
	private final List<UnarySelector> unarySelectors;
	private final List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions;

	BinaryObjectDescription(Primary primary) {
		this.primary = primary;
		this.unarySelectors = new ArrayList<UnarySelector>();
		this.binarySelectorUnaryObjectDescriptions = new ArrayList<BinarySelectorUnaryObjectDescription>();
	}

	void add(UnarySelector unarySelector) {
		unarySelectors.add(unarySelector);
	}

	void add(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		add(new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription));
	}

	void add(BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription) {
		binarySelectorUnaryObjectDescriptions.add(binarySelectorUnaryObjectDescription);
	}

	Primary primary() {
		return primary;
	}

	List<UnarySelector> unarySelectors() {
		return  unarySelectors;
	}

	List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions() {
		return binarySelectorUnaryObjectDescriptions;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this);
		if (primary != null)
			primary.accept(nodeVisitor);
		for (UnarySelector unarySelector : unarySelectors)
			unarySelector.accept(nodeVisitor);
		for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions())
			binarySelectorUnaryObjectDescription.accept(nodeVisitor);
	}
}
