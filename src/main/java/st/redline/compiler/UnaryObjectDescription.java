/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class UnaryObjectDescription implements VisitableNode {

	private final Primary primary;
	private final List<UnarySelector> unarySelectors;

	UnaryObjectDescription(Primary primary) {
		this.primary = primary;
		this.unarySelectors = new ArrayList<UnarySelector>();
	}

	void add(UnarySelector unarySelector) {
		unarySelectors.add(unarySelector);
	}

	Primary primary() {
		return primary;
	}

	List<UnarySelector> unarySelectors() {
		return unarySelectors;
	}

	public void accept(NodeVisitor nodeVisitor) {
	}
}
