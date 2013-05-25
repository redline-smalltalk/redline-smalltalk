/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class BinarySelectorUnaryObjectDescription {

    private final BinarySelector binarySelector;
    private final UnaryObjectDescription unaryObjectDescription;

    public BinarySelectorUnaryObjectDescription(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
        this.binarySelector = binarySelector;
        this.unaryObjectDescription = unaryObjectDescription;
    }

    public BinarySelector binarySelector() {
        return binarySelector;
    }

    public UnaryObjectDescription unaryObjectDescription() {
        return unaryObjectDescription;
    }

    public void accept(NodeVisitor nodeVisitor) {
        if (unaryObjectDescription != null)
            unaryObjectDescription.accept(nodeVisitor);
        if (binarySelector != null)
            binarySelector.accept(nodeVisitor);
    }

    boolean hasBlockWithAnswerExpression() {
        return (unaryObjectDescription != null && unaryObjectDescription.hasBlockWithAnswerExpression());
    }
}
