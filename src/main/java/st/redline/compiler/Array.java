/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Array implements VisitableNode {

	private final List<VisitableNode> visitableNodes;

	public Array() {
		visitableNodes = new ArrayList<VisitableNode>();
	}

	public void add(VisitableNode visitableNode) {
		visitableNodes.add(visitableNode);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
	}
}
