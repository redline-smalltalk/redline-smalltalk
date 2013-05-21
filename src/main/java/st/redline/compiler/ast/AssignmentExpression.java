/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class AssignmentExpression implements Expression {

    private final Identifier identifier;
    private final Expression expression;

    public AssignmentExpression(Identifier identifier, Expression expression) {
        this.identifier = identifier;
        this.identifier.onStoreSideOfExpression();
        this.expression = expression;
        this.expression.leaveResultOnStack();
    }

    public String variableName() {
        return identifier.value();
    }

    public Expression expression() {
        return expression;
    }

    public int line() {
        return identifier.line();
    }

    public void leaveResultOnStack() {
        expression.duplicateResultOnStack();
    }

    public void duplicateResultOnStack() {
        throw new IllegalStateException("Assignment asked to duplicate stack top!");
    }

    public boolean isAnswerExpression() {
        return false;
    }

    public boolean hasBlockWithAnswerExpression() {
        return expression.hasBlockWithAnswerExpression();
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        // Note: The order of visiting below is important. Expression before variable we store result into.
        if (expression != null) {
            expression.accept(nodeVisitor);
        }
        if (identifier != null)
            identifier.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }
}
