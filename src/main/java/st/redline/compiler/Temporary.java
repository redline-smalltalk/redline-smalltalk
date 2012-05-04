/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class Temporary extends Identifier implements VisitableNode {

    Temporary(String value, int line) {
        super(value, line);
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, value(), line());
    }
}
