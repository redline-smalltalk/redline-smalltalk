/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class BinarySelector extends ValuePrimary {

    public BinarySelector() {
        super("", 0);
    }

    public void add(String value, int line) {
        line(line);
        value(value() + value);
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, value(), line());
    }
}
