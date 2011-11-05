/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public interface IndexedVisitableNode extends VisitableNode {
	void index(int index);
	int index();
}
