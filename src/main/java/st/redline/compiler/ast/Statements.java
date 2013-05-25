/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class Statements implements VisitableNode {

    private final Expression expression;
    private final Statements statements;

    public Statements(Expression expression, Statements statements) {
        this.expression = expression;
        this.statements = statements;
        if (statements == null && expression != null)
            expression.leaveResultOnStack();
    }

    public int line() {
        return expression.line();
    }

    public Expression expression() {
        return expression;
    }

    public Statements statements() {
        return statements;
    }

    public boolean hasAnswerExpression() {
        return expression.isAnswerExpression() || (statements != null && statements.hasAnswerExpression());
    }

    public boolean hasBlockWithAnswerExpression() {
        return expression.hasBlockWithAnswerExpression() || (statements != null && statements.hasBlockWithAnswerExpression());
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        if (expression != null)
            expression.accept(nodeVisitor);
        if (statements != null)
            statements.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }
}
