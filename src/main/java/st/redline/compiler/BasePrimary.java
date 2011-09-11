/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public abstract class BasePrimary implements Primary {

	public int line() {
		return 0;
	}

	public String value() {
		return "";
	}
}
