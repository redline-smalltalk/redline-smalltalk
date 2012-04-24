/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;

class Array extends Primary implements ArrayElement {

    private final ArrayList<ArrayElement> elements;
    private int index;
    private int elementIndex = 0;
    private boolean insideArray = false;

    Array() {
        elements = new ArrayList<ArrayElement>();
    }

    void add(ArrayElement arrayElement) {
        if (arrayElement != null) {
            arrayElement.insideArray();
            arrayElement.index(++elementIndex);
            elements.add(arrayElement);
        }
    }

    int size() {
        return elements.size();
    }

    ArrayElement get(int index) {
        return elements.get(index);
    }

    public int index() {
        return index;
    }

    public void insideArray() {
        insideArray = true;
    }

    public boolean isInsideArray() {
        return insideArray;
    }

    public void index(int index) {
        this.index = index;
    }

    public String value() {
        return "<array>";
    }

    public int line() {
        return elements.get(0).line();
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this);
        for (ArrayElement arrayElement : elements)
            arrayElement.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }
}
