/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.List;

public class BlockArguments implements VisitableNode {

    private final List<BlockArgument> blockArguments;

    public BlockArguments(List<BlockArgument> blockArguments) {
        this.blockArguments = blockArguments;
    }

    public boolean isEmpty() {
        return blockArguments.isEmpty();
    }

    public int size() {
        return blockArguments != null ? blockArguments.size() : 0;
    }

    public BlockArgument get(int index) {
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
