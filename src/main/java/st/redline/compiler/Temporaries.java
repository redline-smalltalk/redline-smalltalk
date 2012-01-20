package st.redline.compiler;

import java.util.List;

class Temporaries implements VisitableNode {

	private final List<Temporary> temporaries;

	Temporaries(List<Temporary> temporaries) {
		this.temporaries = temporaries;
	}

	boolean isEmpty() {
		return temporaries.isEmpty();
	}

	int size() {
		return temporaries != null ? temporaries.size() : 0;
	}

	Temporary get(int index) {
		return temporaries.get(index);
	}

	public void accept(NodeVisitor nodeVisitor) {
		if (temporaries == null)
			return;
		nodeVisitor.visitBegin(this);
		for (Temporary temporary : temporaries)
			temporary.accept(nodeVisitor);
		nodeVisitor.visitEnd(this);
	}
}
