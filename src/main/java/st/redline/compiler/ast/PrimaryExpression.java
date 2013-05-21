/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class PrimaryExpression extends Primary {

    private final Expression expression;

    public PrimaryExpression(Expression expression) {
        this.expression = expression;
        expression.leaveResultOnStack();
    }

    public int line() {
        return expression != null ? expression.line() : 0;
    }

    public boolean isBlockWithAnswerExpression() {
        return false;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, line());
        if (expression != null)
            expression.accept(nodeVisitor);
    }
}
