/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class PrimaryStatements extends Primary {

    private final Statements statements;

    public PrimaryStatements(Statements statements) {
        this.statements = statements;
    }

    public int line() {
        return statements != null ? statements.line() : 0;
    }

    public boolean isBlockWithAnswerExpression() {
        return false;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, line());
        if (statements != null)
            statements.accept(nodeVisitor);
    }
}
