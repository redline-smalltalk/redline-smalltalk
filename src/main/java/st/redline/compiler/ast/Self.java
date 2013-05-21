/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class Self implements ReservedWord {
    public void accept(NodeVisitor nodeVisitor, int line) {
        nodeVisitor.visit(this, line);
    }
}
