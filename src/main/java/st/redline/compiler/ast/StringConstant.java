/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public class StringConstant extends ValuePrimary implements ArrayElement {

    private int index;
    private boolean insideArray = false;

    public StringConstant(String value, int line) {
        super(homgenize(value), line);
    }

    private static String homgenize(String value) {
        return value.substring(1, value.length() - 1).replaceAll("''", "'");
    }

    public void insideArray() {
        insideArray = true;
    }

    public boolean isInsideArray() {
        return insideArray;
    }

    public int index() {
        return index;
    }

    public void index(int index) {
        this.index = index;
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visit(this, value(), index, insideArray, line());
    }
}
