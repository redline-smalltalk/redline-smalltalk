/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.List;

public class Temporaries implements VisitableNode {

    private final List<Temporary> temporaries;

    public Temporaries(List<Temporary> temporaries) {
        this.temporaries = temporaries;
    }

    public boolean isEmpty() {
        return temporaries.isEmpty();
    }

    public int size() {
        return temporaries != null ? temporaries.size() : 0;
    }

    public Temporary get(int index) {
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
