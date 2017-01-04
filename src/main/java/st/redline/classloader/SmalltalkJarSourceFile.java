/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import java.io.*;

public class SmalltalkJarSourceFile extends SmalltalkSourceFile {

    public SmalltalkJarSourceFile(String name, String filename, File file, String classpath, SourceReader reader) {
        super(name, filename, file, classpath, reader);
    }

    public String className() {
        if (className == null)
            className = withoutPackage(fullClassName());
        return className;
    }

    public String fullClassName() {
        if (fullClassName == null)
            fullClassName = withoutExtension(name()).replaceAll(String.valueOf(File.separatorChar), ".");
        return fullClassName;
    }

    public String packageName() {
        if (packageName == null) {
            packageName = fullClassName();
            int index = packageName.lastIndexOf(".");
            if (index != -1)
                packageName = packageName.substring(0, index);
        }
        return packageName;
    }

    private String withoutPackage(String fullName) {
        int index = fullName.lastIndexOf(".");
        if (index != -1)
            return fullName.substring(index + 1);
        return fullName;
    }
}
