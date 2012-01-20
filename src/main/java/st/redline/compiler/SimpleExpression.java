/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class SimpleExpression implements Expression {

	private Primary primary;
	private MessageExpression messageExpression;
	private List<MessageElement> messageElements;

	SimpleExpression() {
		messageElements = new ArrayList<MessageElement>();
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

	public void accept(NodeVisitor nodeVisitor) {
	}
}
