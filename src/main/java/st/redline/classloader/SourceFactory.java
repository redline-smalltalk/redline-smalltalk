/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import st.redline.RedlineFile;
import st.redline.classloader.io.FileSourceReader;
import st.redline.classloader.io.JarSourceReader;
import st.redline.classloader.io.SourceReader;

public class SourceFactory {

    public Source createFromFile(String sourceName, String filePath) {
        SourceReader sourceReader = fileSourceReader(sourceName, filePath);
        return createSource(sourceName, filePath, sourceReader);
    }

    public Source createFromJar(String sourceName, String jarPath) {
        SourceReader sourceReader = jarSourceReader(sourceName, jarPath);
        return createSource(sourceName, jarPath, sourceReader);
    }

    private Source createSource(String name, String path, SourceReader reader) {
        return new Source(name, path, reader);
    }

    private SourceReader fileSourceReader(String sourceName, String filePath) {
        String filename = filePath + RedlineFile.separator + sourceName;
        return new FileSourceReader(filename);
    }

    private SourceReader jarSourceReader(String entryName, String jarPath) {
        return new JarSourceReader(entryName, jarPath);
    }
}
