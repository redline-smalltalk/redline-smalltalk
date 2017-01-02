package st.redline.classloader;

import java.io.*;

import static st.redline.classloader.SmalltalkSourceFile.SOURCE_EXTENSION;

public class SourceFactory {

    public Source createFromFile(String sourceName, File file) {
        SourceReader sourceReader = fileSourceReader(file);
        return createSource(sourceName, file, sourceReader);
    }

    public Source createFromJar(String sourceName, String jarPath) {
        SourceReader sourceReader = jarSourceReader(sourceName, jarPath);
        return createSource(sourceName, new File(jarPath), sourceReader);
    }

    private Source createSource(String name, File file, SourceReader reader) {
        return new SmalltalkSourceFile(nameWithoutExtension(file), name, file, reader);
    }

    private String nameWithoutExtension(File file) {
        return file.getName().substring(0, file.getName().lastIndexOf(SOURCE_EXTENSION));
    }

    private SourceReader fileSourceReader(File file) {
        return new FileSourceReader(file);
    }

    private SourceReader jarSourceReader(String entryName, String jarPath) {
        return new JarSourceReader(entryName, jarPath);
    }
}
