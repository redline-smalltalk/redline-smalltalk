/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Symbol implements VisitableNode {

	public final int line;
	public final String value;
	private Identifier identifier;
	private BinarySelector binarySelector;
	private Keyword keyword;

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
}
