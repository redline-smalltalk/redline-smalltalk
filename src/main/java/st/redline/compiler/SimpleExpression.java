/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class SimpleExpression implements Expression {

	public static Cascade cascade = new Cascade();

	private Primary primary;
	private MessageExpression messageExpression;
	private List<MessageElement> messageElements;
	private boolean resultLeftOnStack;
	private boolean duplicateResultOnStack;
    private Object l0;
    private Object l1;
    private Object l2;

	SimpleExpression() {
		messageElements = new ArrayList<MessageElement>();
		resultLeftOnStack = false;
		duplicateResultOnStack = false;
	}

	boolean isResultLeftOnStack() {
		return resultLeftOnStack;
	}

	public void leaveResultOnStack() {
		resultLeftOnStack = true;
	}

	boolean isResultDuplicatedOnStack() {
		return duplicateResultOnStack;
	}

	public void duplicateResultOnStack() {
		duplicateResultOnStack = true;
	}

	void add(Primary primary) {
		this.primary = primary;
	}

	void add(MessageExpression messageExpression) {
		this.messageExpression = messageExpression;
	}

	void add(MessageElement messageElement) {
		this.messageElements.add(messageElement);
	}

	Primary primary() {
		return primary;
	}

	MessageExpression messageExpression() {
		return messageExpression;
	}

	List<MessageElement> messageElements() {
		return messageElements;
	}

	public int line() {
		return primary.line();
	}

    public boolean isAnswerExpression() {
        return false;
    }

    boolean hasBlockWithAnswerExpression() {
        return primary.isBlockWithAnswerExpression()
               || (messageExpression != null && messageExpression.hasBlockWithAnswerExpression())
               || (messageElementsHaveBlockWithAnswerExpression());
    }

    boolean messageElementsHaveBlockWithAnswerExpression() {
        for (MessageElement messageElement : messageElements)
            if (messageElement.hasBlockWithAnswerExpression())
                return true;
        return false;
    }

    Object label0() {
        return l0;
    }

    void label0(Object l0) {
        this.l0 = l0;
    }

    Object label1() {
        return l1;
    }

    void label1(Object l1) {
        this.l1 = l1;
    }

    Object label2() {
        return l2;
    }

    void label2(Object l2) {
        this.l2 = l2;
    }

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visitBegin(this);
		if (primary != null)
			primary.accept(nodeVisitor);
		if (messageExpression != null)
			messageExpression.accept(nodeVisitor);
		// We duplicate the stack top for cascaded expressions and don't always pop them off.
		// This is ok, JVM balances the stack (checked with ASM folks).
		int countOfMessageElements = messageElements.size();
		for (int index = 0; index < countOfMessageElements; index++) {
			cascade.begin(nodeVisitor);
			messageElements.get(index).accept(nodeVisitor);
			if (index + 1 < countOfMessageElements)
				cascade.end(nodeVisitor);
		}
		nodeVisitor.visitEnd(this);
	}
}
