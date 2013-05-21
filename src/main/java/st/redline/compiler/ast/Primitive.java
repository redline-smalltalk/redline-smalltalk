/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class Primitive extends Primary {

    private final String keyword;
    private final int line;
    private final String digits;

    public Primitive(String keyword, int line, String digits) {
        this.keyword = keyword;
        this.line = line;
        this.digits = digits;
    }

    public int line() {
        return line;
    }

    public String value() {
        return keyword;
    }

    public String digits() {
        return digits;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, keyword, line, digits);
    }
}
