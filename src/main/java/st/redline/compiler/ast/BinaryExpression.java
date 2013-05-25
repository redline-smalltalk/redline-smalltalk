/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.ArrayList;
import java.util.List;

public class BinaryExpression implements MessageExpression {

    private final List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions;
    private KeywordExpression keywordExpression;

    public BinaryExpression() {
        binarySelectorUnaryObjectDescriptions = new ArrayList<BinarySelectorUnaryObjectDescription>();
    }

    public void add(KeywordExpression keywordExpression) {
        this.keywordExpression = keywordExpression;
    }

    public KeywordExpression keywordExpression() {
        return keywordExpression;
    }

    public void add(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
        binarySelectorUnaryObjectDescriptions.add(new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription));
    }

    public List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions() {
        return binarySelectorUnaryObjectDescriptions;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions())
            binarySelectorUnaryObjectDescription.accept(nodeVisitor);
        if (keywordExpression != null)
            keywordExpression.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }

    public boolean hasBlockWithAnswerExpression() {
        return (keywordExpression != null && keywordExpression.hasBlockWithAnswerExpression())
                || binarySelectorUnaryObjectDescriptionsHaveBlockWithAnswerExpression();
    }

    boolean binarySelectorUnaryObjectDescriptionsHaveBlockWithAnswerExpression() {
        for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions())
            if (binarySelectorUnaryObjectDescription.hasBlockWithAnswerExpression())
                return true;
        return false;
    }
}
