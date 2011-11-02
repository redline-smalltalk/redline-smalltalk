/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Temporaries implements VisitableNode {

	private final List<Temporary> temporaries;
	private int offset = 0;

	public Temporaries() {
		temporaries = new ArrayList<Temporary>();
	}

	public boolean isEmpty() {
		return temporaries.isEmpty();
	}

	public void add(Temporary temporary) {
		temporary.index(offset++);
		temporaries.add(temporary);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		for (Temporary temporary : temporaries)
			temporary.accept(visitor);
		visitor.visitEnd(this);
	}

	public int size() {
		return temporaries.size();
	}
}
