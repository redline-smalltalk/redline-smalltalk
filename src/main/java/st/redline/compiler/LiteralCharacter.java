/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class LiteralCharacter extends BaseLiteral {

	private final CharacterConstant characterConstant;

	public LiteralCharacter(CharacterConstant characterConstant) {
		this.characterConstant = characterConstant;
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, characterConstant.value, characterConstant.line);
	}
}
