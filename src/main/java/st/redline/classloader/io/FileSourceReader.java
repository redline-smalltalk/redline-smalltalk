/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader.io;

import java.io.*;

public class FileSourceReader implements SourceReader {

    private String filename;

    public FileSourceReader(String filename) {
        this.filename = filename;
    }

    public String contents() {
        FileInputStream fis = null;
        String newLine = System.getProperty("line.separator");
        StringBuilder contents = new StringBuilder();
        try {
            fis = new FileInputStream(filename);
            BufferedReader reader = new BufferedReader(new InputStreamReader(fis));
            String line;
            while ((line = reader.readLine()) != null) {
                contents.append(line);
                contents.append(newLine);
            }
            return contents.toString();
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            close(fis);
        }
    }

    private void close(InputStream inputStream) {
        if (inputStream != null)
            try {
                inputStream.close();
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
    }
}
