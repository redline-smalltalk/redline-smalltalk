/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

public class InMemorySourceFile extends SourceFile {

    private static final String NAME_HINT = "\"@:";
    private final String contents;

    public InMemorySourceFile(String contents) {
        super(System.getProperty("user.dir"));
        this.contents = contents;
    }

    public String contents() {
        return contents;
    }

    public String shortName() {
        if (contents.startsWith(NAME_HINT))
            return shortNameFromEmbeddedHint();
        return "S" + contents.hashCode();
    }

    private String shortNameFromEmbeddedHint() {
        String name = embeddedName();
        return name.substring(name.lastIndexOf('.') + 1);
    }

    public String packageName() {
        if (contents.startsWith(NAME_HINT))
            return packageNameFromEmbeddedHint();
        return "evaled";
    }

    private String packageNameFromEmbeddedHint() {
        String name = embeddedName();
        return name.substring(0, name.lastIndexOf('.'));
    }

    private String embeddedName() {
        return contents().substring(3, contents.indexOf("\"", 3)).trim();
    }
}
