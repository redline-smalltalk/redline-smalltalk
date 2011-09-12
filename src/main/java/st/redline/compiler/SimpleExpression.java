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

	public void add(Primary primary) {
		this.primary = primary;
	}

	public void add(MessageExpression messageExpression) {
		this.messageExpression = messageExpression;
	}

	public void add(MessageElement messageElement) {
		messageElements.add(messageElement);
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
