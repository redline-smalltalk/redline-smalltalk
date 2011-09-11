/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class StringChunk extends StringConstant {

	public StringChunk(String value, int line) {
		super(value, line);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}
}
