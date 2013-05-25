/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

public abstract class ValuePrimary extends Primary {

    private String value;
    private int line;

    public ValuePrimary(String value, int line) {
        this.value = value;
        this.line = line;
    }

    public String value() {
        return value;
    }

    public void value(String value) {
        this.value = value;
    }

    public int line() {
        return line;
    }

    public void line(int line) {
        this.line = line;
    }
}
