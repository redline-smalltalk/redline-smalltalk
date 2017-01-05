/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import java.io.*;
import java.util.jar.*;
import java.util.zip.*;

public class JarSourceReader implements SourceReader {

    private final String entryName;
    private final String path;

    public JarSourceReader(String entryName, String path) {
        this.entryName = entryName;
        this.path = path;
    }

    public String contents(LineTransformer lineTransformer) {
        // TODO.JCL - There is a lot of commonality between this 'contents()' method
        // and the one in FileSourceReader - DRY up into SourceReader at some point.
        String newLine = System.getProperty("line.separator");
        JarFile jarFile = tryCreateJarFile();
        ZipEntry entry = jarFile.getEntry(entryName);
        if (entry == null)
            throw new IllegalStateException("Expected JAR entry '" + entryName + "' not found in: " + path);
        StringBuilder contents = new StringBuilder();
        BufferedReader reader = null;
        try {
            InputStream input = jarFile.getInputStream(entry);
            InputStreamReader isr = new InputStreamReader(input);
            reader = new BufferedReader(isr);
            String line;
            line = lineTransformer.begin();
            if (line != null)
                contents.append(line);
            while ((line = reader.readLine()) != null) {
                contents.append(lineTransformer.transform(line + newLine));
            }
            line = lineTransformer.end();
            if (line != null)
                contents.append(line);
            return contents.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            close(reader);
        }
    }

    private void close(Reader reader) {
        if (reader != null)
            try {
                reader.close();
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
    }

    private JarFile tryCreateJarFile() {
        try {
            return new JarFile(path);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
