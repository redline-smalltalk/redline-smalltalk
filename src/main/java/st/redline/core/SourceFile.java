/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import java.io.File;

public class SourceFile extends File {

    public SourceFile(File file) {
        super(file.getAbsolutePath());
    }

    protected SourceFile(String pathname) {
        super(pathname);
    }

    public String contents() {
        return sourceFileReader().read(this);
    }

    private SourceFileReader sourceFileReader() {
        return new SourceFileReader();
    }

    public String shortName() {
        String name = name();
        // assumes we have an file extension.
        return ClassPathUtilities.filenameToClassName(name);
    }

    public String packageName() {
        String name = name();
        return ClassPathUtilities.filenameToPackageName(name);
    }

    private String name() {
        String name = toString();
        String userDir = userDir();
        if (name.startsWith(userDir))
            name = name.substring(userDir.length() + 1);
        if (name.startsWith("rt"))
            return name.substring(3);
        return name;
    }

    protected String userDir() {
        return System.getProperty("user.dir");
    }
}
