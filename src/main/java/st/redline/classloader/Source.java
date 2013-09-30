/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import st.redline.RedlineFile;
import st.redline.classloader.io.SourceReader;

import java.io.File;

public class Source {

    private final String name;
    private final String path;
    private final SourceReader reader;
    private final String className;
    private final String packageName;

    public Source(String name, String path, SourceReader reader) {
        this.name = name;
        this.path = path;
        this.reader = reader;
        this.className = makeClassName();
        this.packageName = makePackageName();
    }

    private String makePackageName() {
        int index = name.lastIndexOf(RedlineFile.separator);
        if (index == -1)
            return "";
        return name.substring(0, index);
    }

    private String makeClassName() {
        int index = name.lastIndexOf(RedlineFile.separator);
        if (index == -1)
            index = 0;
        return name.substring(index + 1, name.lastIndexOf(".st"));
    }

    public String name() {
        return name;
    }

    public String path() {
        return path;
    }

    public String className() {
        return className;
    }

    public String packageName() {
        return packageName;
    }

    public String fullyQualifiedName() {
        return packageName.replace(RedlineFile.separator, ".") + "." + className;
    }

    public SourceReader reader() {
        return reader;
    }

    public String contents() {
        return reader().contents();
    }

    public boolean isNextTo(String otherFile) {
        String filename = path + RedlineFile.separator + name.replace(className + ".st", otherFile + ".st");
        File file = new File(filename);
        return file.exists();
    }
}
