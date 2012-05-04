/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

public class InMemorySourceFile extends SourceFile {

    private final String contents;

    public InMemorySourceFile(String contents) {
        super(System.getProperty("user.dir"));
        this.contents = contents;
    }

    public String contents() {
        return contents;
    }

    public String shortName() {
        return "S" + contents.hashCode();
    }

    public String packageName() {
        return "evaled";
    }
}
