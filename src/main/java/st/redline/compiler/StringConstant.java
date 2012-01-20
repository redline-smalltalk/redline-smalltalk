/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class StringConstant extends ValuePrimary implements ArrayElement {

	StringConstant(java.lang.String value, int line) {
		super(homgenize(value), line);
	}

	private static String homgenize(String value) {
		return value.substring(1, value.length()-1).replaceAll("''", "'");
	}
}
