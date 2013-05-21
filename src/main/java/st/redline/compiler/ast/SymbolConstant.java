/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class SymbolConstant extends ValuePrimary {

    public SymbolConstant(String value, int line) {
        super(value, line);
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, value(), line());
    }
}
