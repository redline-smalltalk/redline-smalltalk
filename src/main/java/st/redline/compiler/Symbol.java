/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Symbol implements IndexedVisitableNode {

	public final int line;
	public final String value;
	private Identifier identifier;
	private BinarySelector binarySelector;
	private Keyword keyword;
	private int index = 0;

	public Symbol(Identifier identifier) {
		this.identifier = identifier;
		value = identifier.value;
		line = identifier.line;
	}

	public Symbol(BinarySelector binarySelector) {
		this.binarySelector = binarySelector;
		value = binarySelector.value;
		line = binarySelector.line;
	}

	public Symbol(Keyword keyword) {
		this.keyword = keyword;
		value = keyword.value;
		line = keyword.line;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}

	public void index(int index) {
		this.index = index;
	}

	public int index() {
		return index;
	}
}
