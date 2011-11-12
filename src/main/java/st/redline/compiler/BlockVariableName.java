/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class BlockVariableName extends VariableName {

	public BlockVariableName(VariableName variableName) {
		super(variableName.value(), variableName.line());
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, value, line);
	}
}
