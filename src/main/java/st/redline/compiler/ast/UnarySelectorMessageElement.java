/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class UnarySelectorMessageElement implements MessageElement {

    private final UnarySelector unarySelector;

    public UnarySelectorMessageElement(UnarySelector unarySelector) {
        this.unarySelector = unarySelector;
    }

    public UnarySelector unarySelector() {
        return unarySelector;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, unarySelector.value(), unarySelector.line());
    }

    public boolean hasBlockWithAnswerExpression() {
        return false;
    }
}
