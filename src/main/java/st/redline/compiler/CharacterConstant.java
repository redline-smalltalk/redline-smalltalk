/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class CharacterConstant extends ValuePrimary implements ArrayElement {

	CharacterConstant(String value, int line) {
		super(value, line);
	}
}
