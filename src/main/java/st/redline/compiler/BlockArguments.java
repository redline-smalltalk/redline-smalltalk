package st.redline.compiler;

import java.util.List;

class BlockArguments implements VisitableNode {

	private final List<BlockArgument> blockArguments;

	BlockArguments(List<BlockArgument> blockArguments) {
		this.blockArguments = blockArguments;
	}

	boolean isEmpty() {
		return blockArguments.isEmpty();
	}

	int size() {
		return blockArguments.size();
	}

	BlockArgument get(int index) {
		return blockArguments.get(index);
	}

	public void accept(NodeVisitor nodeVisitor) {
	}
}
