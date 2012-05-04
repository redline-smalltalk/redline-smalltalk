/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
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
        return blockArguments != null ? blockArguments.size() : 0;
    }

    BlockArgument get(int index) {
        return blockArguments.get(index);
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this, size());
        if (blockArguments != null)
            for (BlockArgument blockArgument : blockArguments)
                blockArgument.accept(nodeVisitor);
        nodeVisitor.visitEnd(this, size());
    }
}
