/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class ReferencedClass extends Identifier implements VisitableNode {

    public ReferencedClass(String value) {
        super(value, 0);
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, value());
    }
}
