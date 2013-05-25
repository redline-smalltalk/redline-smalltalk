/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.ArrayList;
import java.util.List;

public class UnaryExpression implements MessageExpression {

    private List<UnarySelector> unarySelectors;
    private MessageExpression messageExpression;

    public UnaryExpression() {
        unarySelectors = new ArrayList<UnarySelector>();
    }

    public void add(UnarySelector unarySelector) {
        unarySelectors.add(unarySelector);
    }

    public List<UnarySelector> unarySelectors() {
        return unarySelectors;
    }

    public void add(MessageExpression messageExpression) {
        this.messageExpression = messageExpression;
    }

    public MessageExpression messageExpression() {
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
