/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Program implements VisitableNode {

    private Temporaries temporaries;
    private Statements statements;

    Program(Temporaries temporaries, Statements statements) {
        this.temporaries = temporaries;
        this.statements = statements;
    }

    Statements statements() {
        return statements;
    }

    Temporaries temporaries() {
        return temporaries;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        if (temporaries != null)
            temporaries.accept(nodeVisitor);
        if (statements != null)
            statements.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }
}
