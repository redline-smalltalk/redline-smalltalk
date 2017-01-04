/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/* Note: Move the preprocessing responsibility into an inner class. */

public class SmalltalkSourceFile implements Source, LineTransformer {

    public static final String SEPARATOR = "/";
    public static final String SOURCE_EXTENSION = ".st";

    private static final Pattern METHOD_START_PATTERN = Pattern.compile("^[-+] .*\\s");
    private static final Pattern METHOD_UNARY_PATTERN = Pattern.compile("^\\w+[^:|\\s]");
    private static final Pattern METHOD_BINARY_PATTERN = Pattern.compile("^[-\\\\+*/=><,@%~|&?]+ \\w+");
    private static final Pattern METHOD_KEYWORD_PATTERN = Pattern.compile("(\\w+: \\w+)+");
    private static final String METHOD_START = "[";
    private static final String METHOD_END = "].";
    private static final String CLASS_SELECTOR = "class";
    private static final String METHOD_AT_SELECTOR = "basicAddSelector:";
    private static final String METHOD_PUT_SELECTOR = "withMethod:";
    private static final String CLASS_METHOD_INDICATOR = "+ ";
    private static final String NEWLINE = System.getProperty("line.separator");

    private final String name;
    private final String filename;
    private final File file;
    private final String classpath;
    private final SourceReader reader;
    private boolean methods;
    protected String className;
    protected String selector;
    protected String arguments;
    protected String fullClassName;
    protected String packageName;

    public SmalltalkSourceFile(String name, String filename, File file, String classpath, SourceReader reader) {
        this.name = name;
        this.filename = filename;
        this.file = file;
        this.classpath = classpath;
        this.reader = reader;
        this.methods = false;
    }

    public boolean hasContent() {
        return file.length() > 0;
    }

    public String contents() {
        return reader.contents(this);
    }

    public String transform(String line) {
        if (isMethodDefinition(line))
            return methodDefinitionTransformation(line);
        return line;
    }

    public String fileExtension() {
        return SOURCE_EXTENSION;
    }

    private String methodDefinitionTransformation(String line) {
        StringBuffer transformation = new StringBuffer();
        if (hasMethods())
            transformation.append(METHOD_END + " ");
        transformation.append(className());
        if (isClassMethod(line))
            transformation.append(" " + CLASS_SELECTOR);
        transformation.append(" " + METHOD_AT_SELECTOR);
        extractSelectorAndArgumentsFrom(line);
        transformation.append(" #" + selector());
        transformation.append(" " + METHOD_PUT_SELECTOR);
        transformation.append(" " + METHOD_START);
        if (arguments.length() > 0)
            transformation.append(arguments + " |");
        transformation.append(NEWLINE);
        methods = true;
        return transformation.toString();
    }

    private String selector() {
        return selector;
    }

    private void extractSelectorAndArgumentsFrom(String line) {
        String input = line;
        if (input.startsWith("+ ") || input.startsWith("- "))
            input = input.substring(2);
        arguments = "";
        if (!isKeywordSelector(input) && !isUnarySelector(input) && !isBinarySelector(input))
            selector = "UnknownSelector";
    }

    private boolean isUnarySelector(String input) {
        Matcher matcher = METHOD_UNARY_PATTERN.matcher(input);
        if (matcher.find()) {
            selector = matcher.group();
            return true;
        }
        return false;
    }

    private boolean isBinarySelector(String input) {
        Matcher matcher = METHOD_BINARY_PATTERN.matcher(input);
        if (matcher.find()) {
            selector = matcher.group();
            int space = selector.indexOf(' ');
            arguments = " :" + selector.substring(space + 1);
            selector = selector.substring(0, space);
            return true;
        }
        return false;
    }

    private boolean isKeywordSelector(String input) {
        Matcher matcher = METHOD_KEYWORD_PATTERN.matcher(input);
        selector = "";
        arguments = "";
        boolean found = false;
        while (matcher.find()) {
            String keyword = matcher.group();
            int space = keyword.indexOf(' ');
            arguments = arguments + " :" + keyword.substring(space + 1);
            selector = selector + keyword.substring(0, space);
            found = true;
        }
        return found;
    }

    private boolean isClassMethod(String line) {
        return line.startsWith(CLASS_METHOD_INDICATOR);
    }

    public String classpath() {
        return classpath;
    }

    public String name() {
        return name;
    }

    public String className() {
        if (className == null)
            className = withoutExtension(filename());
        return className;
    }

    public String fullClassName() {
        if (fullClassName == null)
            fullClassName = withoutClassPath(withoutExtension(fullFilename()));
        return fullClassName;
    }

    public String packageName() {
        if (packageName == null) {
            packageName = fullClassName();
            int index = packageName.lastIndexOf(File.separatorChar);
            if (index != -1)
                packageName = packageName.substring(0, index);
            packageName = packageName.replaceAll(String.valueOf(File.separatorChar), ".");
        }
        return packageName;
    }

    private String withoutClassPath(String filename) {
        if (classpath.length() > 0 && filename.startsWith(classpath))
            return filename.substring(classpath.length() + 1);
        return filename;
    }

    protected String withoutExtension(String filename) {
        return filename.substring(0, filename.lastIndexOf("."));
    }

    private boolean isMethodDefinition(String line) {
        return METHOD_START_PATTERN.matcher(line).matches();
    }

    public String filename() {
        return file.getName();
    }

    public String fullFilename() {
        return file.getPath();
    }

    public String begin() {
        return "";
    }

    public String end() {
        return hasMethods() ? METHOD_END + NEWLINE : "";
    }

    private String classInitializer() {
        return className() + " initialize." + NEWLINE;
    }

    private boolean hasMethods() {
        return methods;
    }
}
