/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public abstract class Primary implements VisitableNode {

	abstract int line();

    public boolean isBlockWithAnswerExpression() {
        return false;
    }
}
