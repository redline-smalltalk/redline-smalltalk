/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class Primitive extends Primary {

    private final String keyword;
    private final int line;
    private final String digits;

    Primitive(String keyword, int line, String digits) {
        this.keyword = keyword;
        this.line = line;
        this.digits = digits;
    }

    int line() {
        return line;
    }

    String value() {
        return keyword;
    }

    String digits() {
        return digits;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, keyword, line, digits);
    }
}
