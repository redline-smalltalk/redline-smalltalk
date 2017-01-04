/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import java.io.*;

import static st.redline.classloader.SmalltalkSourceFile.SOURCE_EXTENSION;

public class SourceFactory {

    public Source createFromFile(String sourceName, File file, String classpath) {
        SourceReader sourceReader = fileSourceReader(file);
        return new SmalltalkSourceFile(nameWithoutExtension(file), sourceName, file, classpath, sourceReader);
    }

    public Source createFromJar(String sourceName, String jarPath) {
        SourceReader sourceReader = jarSourceReader(sourceName, jarPath);
        return new SmalltalkJarSourceFile(sourceName, jarPath, new File(jarPath), jarPath, sourceReader);
    }

    private String nameWithoutExtension(File file) {
        int index = file.getName().lastIndexOf(SOURCE_EXTENSION);
        if (index == -1)
            return file.getName();
        return file.getName().substring(0, file.getName().lastIndexOf(SOURCE_EXTENSION));
    }

    private SourceReader fileSourceReader(File file) {
        return new FileSourceReader(file);
    }

    private SourceReader jarSourceReader(String entryName, String jarPath) {
        return new JarSourceReader(entryName, jarPath);
    }
}
