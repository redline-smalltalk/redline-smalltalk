/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class LiteralNumber extends BaseLiteral {

	private final NumberConstant numberConstant;

	public LiteralNumber(NumberConstant numberConstant) {
		this.numberConstant = numberConstant;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, numberConstant.value, numberConstant.line);
	}
		
	@Override
	public int line() {
		return numberConstant.line;
	}

	@Override
	public String value() {
		return numberConstant.value;
	}	
}
