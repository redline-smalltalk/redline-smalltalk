/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import java.io.*;

import static st.redline.classloader.SmalltalkSourceFile.SOURCE_EXTENSION;

public class SourceFactory {

    public Source createFromFile(String sourceName, File file, String classpath) {
        SourceReader sourceReader = fileSourceReader(file);
        return new SmalltalkSourceFile(nameWithoutExtension(file.getName()), sourceName, file, classpath, sourceReader);
    }

    public Source createFromJar(String sourceName, String jarPath) {
        SourceReader sourceReader = jarSourceReader(sourceName, jarPath);
        return new SmalltalkSourceFile(nameWithoutExtensionAndPackage(sourceName), sourceName, new File(jarPath), jarPath, sourceReader);
    }

    private String nameWithoutExtensionAndPackage(String sourceName) {
        String name = nameWithoutExtension(sourceName);
        int index = name.lastIndexOf(File.separator);
        if (index == -1)
            return name;
        return name.substring(index + 1);
    }

    private String nameWithoutExtension(String name) {
        int index = name.lastIndexOf(SOURCE_EXTENSION);
        if (index == -1)
            return name;
        return name.substring(0, index);
    }

    private SourceReader fileSourceReader(File file) {
        return new FileSourceReader(file);
    }

    private SourceReader jarSourceReader(String entryName, String jarPath) {
        return new JarSourceReader(entryName, jarPath);
    }
}
