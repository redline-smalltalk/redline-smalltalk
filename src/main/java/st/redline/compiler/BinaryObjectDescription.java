/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class BinaryObjectDescription implements VisitableNode {

	private final Primary primary;
	private final List<UnarySelector> unarySelectors;
	private final List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions;

	public BinaryObjectDescription(Primary primary) {
		this.primary = primary;
		this.unarySelectors = new ArrayList<UnarySelector>();
		this.binarySelectorUnaryObjectDescriptions = new ArrayList<BinarySelectorUnaryObjectDescription>();
	}

	public void add(UnarySelector unarySelector) {
		unarySelectors.add(unarySelector);
	}

	public void add(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		add(new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription));
	}

	protected void add(BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription) {
		binarySelectorUnaryObjectDescriptions.add(binarySelectorUnaryObjectDescription);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		primary.accept(visitor);
		for (UnarySelector unarySelector : unarySelectors)
			unarySelector.accept(visitor);
		for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions)
			binarySelectorUnaryObjectDescription.accept(visitor);
	}
}
