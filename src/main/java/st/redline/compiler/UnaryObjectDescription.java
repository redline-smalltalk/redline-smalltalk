/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class UnaryObjectDescription implements VisitableNode {

	private final Primary primary;
	private final List<UnarySelector> unarySelectors;

	public UnaryObjectDescription(Primary primary) {
		this.primary = primary;
		this.unarySelectors = new ArrayList<UnarySelector>();
	}

	public void add(UnarySelector unarySelector) {
		unarySelectors.add(unarySelector);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		primary.accept(visitor);
		for (UnarySelector unarySelector : unarySelectors)
			unarySelector.accept(visitor);
	}
}
