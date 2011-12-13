/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public abstract class BaseLiteral implements Literal, IndexedVisitableNode {

	private int index = 0;

	public int line() {
		return 0;
	}

	public String value() {
		return "";
	}

	public void index(int index) {
		this.index = index;
	}

	public int index() {
		return index;
	}

	public boolean isBlockWithAnswerExpression() {
		return false;
	}
}
