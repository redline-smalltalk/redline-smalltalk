/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class LiteralArray extends BaseLiteral {

	public final Array array;

	public LiteralArray(Array array) {
		this.array = array;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this);
	}
}
