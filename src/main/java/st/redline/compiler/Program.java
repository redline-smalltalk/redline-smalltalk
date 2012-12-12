/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Program implements VisitableNode {

    private Temporaries temporaries;
    private Statements statements;
    private ReferencedClasses referencedClasses;

    Program(Temporaries temporaries, Statements statements, ReferencedClasses referencedClasses) {
        this.temporaries = temporaries;
        this.statements = statements;
        this.referencedClasses = referencedClasses;
    }

    Statements statements() {
        return statements;
    }

    Temporaries temporaries() {
        return temporaries;
    }

    ReferencedClasses referencedClasses() {
        return referencedClasses;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        if (referencedClasses != null)
            referencedClasses.accept(nodeVisitor);
        if (temporaries != null)
            temporaries.accept(nodeVisitor);
        if (statements != null)
            statements.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }
}
