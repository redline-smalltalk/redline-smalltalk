/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public interface Primary extends VisitableNode {
	int line();
	String value();
}
