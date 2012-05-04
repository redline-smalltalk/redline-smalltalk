/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class UnaryExpression implements MessageExpression {

    private List<UnarySelector> unarySelectors;
    private MessageExpression messageExpression;

    UnaryExpression() {
        unarySelectors = new ArrayList<UnarySelector>();
    }

    void add(UnarySelector unarySelector) {
        unarySelectors.add(unarySelector);
    }

    List<UnarySelector> unarySelectors() {
        return unarySelectors;
    }

    void add(MessageExpression messageExpression) {
        this.messageExpression = messageExpression;
    }

    MessageExpression messageExpression() {
        return messageExpression;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        for (UnarySelector unarySelector : unarySelectors)
            unarySelector.accept(nodeVisitor);
        if (messageExpression != null)
            messageExpression.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }

    public boolean hasBlockWithAnswerExpression() {
        return (messageExpression != null && messageExpression.hasBlockWithAnswerExpression());
    }
}
