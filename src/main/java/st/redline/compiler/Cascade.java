/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class Cascade {

	public void begin(NodeVisitor visitor) {
		visitor.visit(this);
	}

	public void end(NodeVisitor visitor) {
		visitor.visitEnd(this);
	}
}
