/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.ArrayList;
import java.util.List;

public class BinaryObjectDescription implements VisitableNode {

    private final Primary primary;
    private final List<UnarySelector> unarySelectors;
    private final List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions;

    public BinaryObjectDescription(Primary primary) {
        this.primary = primary;
        this.unarySelectors = new ArrayList<UnarySelector>();
        this.binarySelectorUnaryObjectDescriptions = new ArrayList<BinarySelectorUnaryObjectDescription>();
    }

    public void add(UnarySelector unarySelector) {
        unarySelectors.add(unarySelector);
    }

    public void add(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
        add(new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription));
    }

    public void add(BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription) {
        binarySelectorUnaryObjectDescriptions.add(binarySelectorUnaryObjectDescription);
    }

    public Primary primary() {
        return primary;
    }

    public List<UnarySelector> unarySelectors() {
        return  unarySelectors;
    }

    public List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions() {
        return binarySelectorUnaryObjectDescriptions;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this);
        if (primary != null)
            primary.accept(nodeVisitor);
        for (UnarySelector unarySelector : unarySelectors)
            unarySelector.accept(nodeVisitor);
        for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions())
            binarySelectorUnaryObjectDescription.accept(nodeVisitor);
    }

    public boolean hasBlockWithAnswerExpression() {
        return primary.isBlockWithAnswerExpression()
                || binarySelectorUnaryObjectDescriptionsHaveBlockWithAnswerExpression();
    }

    public boolean binarySelectorUnaryObjectDescriptionsHaveBlockWithAnswerExpression() {
        for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions())
            if (binarySelectorUnaryObjectDescription.hasBlockWithAnswerExpression())
                return true;
        return false;
    }
}
