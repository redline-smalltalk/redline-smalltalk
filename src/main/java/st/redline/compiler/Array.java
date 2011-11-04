/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Array implements IndexedVisitableNode {

	private final List<IndexedVisitableNode> visitableNodes;

	private int line;
	private int index = 0;
	private int elementIndex = 0;

	public Array() {
		visitableNodes = new ArrayList<IndexedVisitableNode>();
	}

	public void add(IndexedVisitableNode visitableNode) {
		visitableNode.index(++elementIndex);
		visitableNodes.add(visitableNode);
	}

	public int size() {
		return visitableNodes.size();
	}

	public int line() {
		return line;
	}

	public void line(int line) {
		this.line = line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		for (VisitableNode visitableNode : visitableNodes)
			visitableNode.accept(visitor);
		visitor.visitEnd(this);
	}

	public void index(int index) {
		this.index = index;
	}

	public int index() {
		return index;
	}
}
