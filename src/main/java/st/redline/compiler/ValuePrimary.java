/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

abstract class ValuePrimary extends Primary {

    private String value;
    private int line;

    ValuePrimary(String value, int line) {
        this.value = value;
        this.line = line;
    }

    public String value() {
        return value;
    }

    void value(String value) {
        this.value = value;
    }

    public int line() {
        return line;
    }

    void line(int line) {
        this.line = line;
    }
}
