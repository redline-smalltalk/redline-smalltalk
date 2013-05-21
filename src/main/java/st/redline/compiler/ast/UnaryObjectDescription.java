/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.ArrayList;
import java.util.List;

public class UnaryObjectDescription implements VisitableNode {

    private final Primary primary;
    private final List<UnarySelector> unarySelectors;

    public UnaryObjectDescription(Primary primary) {
        this.primary = primary;
        this.unarySelectors = new ArrayList<UnarySelector>();
    }

    public void add(UnarySelector unarySelector) {
        unarySelectors.add(unarySelector);
    }

    public Primary primary() {
        return primary;
    }

    public List<UnarySelector> unarySelectors() {
        return unarySelectors;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this);
        if (primary != null)
            primary.accept(nodeVisitor);
        for (UnarySelector unarySelector : unarySelectors)
            unarySelector.accept(nodeVisitor);
    }

    public boolean hasBlockWithAnswerExpression() {
        return primary.isBlockWithAnswerExpression();
    }
}
