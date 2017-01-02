package st.redline.classloader;

import java.io.File;

public class SmalltalkSourceFinder implements SourceFinder {

    public Source find(String name) {
        String filename = toFilename(name);
        File file = new File(filename);
        if (!file.exists())
            return new SourceNotFound();
        return sourceFile(name, filename, file);
    }

    private SmalltalkSourceFile sourceFile(String name, String filename, File file) {
        return new SmalltalkSourceFile(name, filename, file, new FileSourceReader(file));
    }

    private String toFilename(String name) {
        return name.replaceAll("\\.", File.separator) + ".st";
    }

    public class SourceNotFound implements Source {

        public boolean hasContent() {
            return false;
        }

        public String contents() {
            return "";
        }

        public String className() {
            return "";
        }

        public String fullClassName() {
            return "";
        }

        public String fileExtension() {
            return "";
        }
    }
}
