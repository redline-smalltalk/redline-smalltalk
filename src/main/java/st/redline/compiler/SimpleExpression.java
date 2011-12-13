/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class SimpleExpression implements Expression {

	private static final Cascade cascade = new Cascade();

	private Primary primary;
	private MessageExpression messageExpression;
	private final List<MessageElement> messageElements;
	private boolean resultLeftOnStack;
	private boolean duplicateResultOnStack;
	private Object l0;
	private Object l1;
	private Object l2;

	public SimpleExpression() {
		messageElements = new ArrayList<MessageElement>();
		resultLeftOnStack = false;
		duplicateResultOnStack = false;
	}

	public boolean isResultLeftOnStack() {
		// System.out.println("isResultLeftOnStack() " + resultLeftOnStack);
		return resultLeftOnStack;
	}

	public void leaveResultOnStack() {
		resultLeftOnStack = true;
	}

	public boolean isResultDuplicatedOnStack() {
		return duplicateResultOnStack;
	}

	public void duplicateResultOnStack() {
		duplicateResultOnStack = true;
	}

	public boolean isAnswerExpression() {
		return false;
	}

	public void add(Primary primary) {
		this.primary = primary;
	}

	public void add(MessageExpression messageExpression) {
		this.messageExpression = messageExpression;
	}

	public void add(MessageElement messageElement) {
		messageElements.add(messageElement);
	}

	public boolean hasBlockWithAnswerExpression() {
		return primary.isBlockWithAnswerExpression();
	}

	public Object label0() {
		return l0;
	}

	public void label0(Object l0) {
		this.l0 = l0;
	}

	public Object label1() {
		return l1;
	}

	public void label1(Object l1) {
		this.l1 = l1;
	}

	public Object label2() {
		return l2;
	}

	public void label2(Object l2) {
		this.l2 = l2;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
		primary.accept(visitor);
		if (messageExpression != null)
			messageExpression.accept(visitor);
		// TODO.jcl WARNING: because we issue a DUP on cascade.begin() and we don't always do a POP on cascade.end()
		// it is possible for the stack to be unbalanced. Checking with ASM people if this is going to be a problem.
		int countOfMessageElements = messageElements.size();
		for (int index = 0; index < countOfMessageElements; index++) {
			cascade.begin(visitor);
			messageElements.get(index).accept(visitor);
			if (index + 1 < countOfMessageElements)
				cascade.end(visitor);
		}
		visitor.visitEnd(this);
	}
}
