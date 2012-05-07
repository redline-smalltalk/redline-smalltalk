/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

public class SmalltalkSourceClassLoader extends ClassLoader {

    private final CommandLine commandLine;

    public SmalltalkSourceClassLoader(ClassLoader classLoader, CommandLine commandLine) {
        super(classLoader);
        this.commandLine = commandLine;
    }

    public Class findClass(String className) throws ClassNotFoundException {
        SourceFile sourceFile = findSource(className);
        if (sourceFile == null)
            return super.findClass(className);
        return classFrom(sourceFile);
    }

    public Class classFrom(SourceFile sourceFile) {
        byte[] classBytes = compile(sourceFile);
        return defineClass(null, classBytes, 0, classBytes.length);
    }

    public Class defineClass(byte[] classBytes) {
        return defineClass(null, classBytes, 0, classBytes.length);
    }

    private byte[] compile(SourceFile sourceFile) {
        return createCompiler(sourceFile).compile();
    }

    private Kompiler createCompiler(SourceFile sourceFile) {
        return new Kompiler(sourceFile, commandLine.verboseRequested(), commandLine.ignoreCompilerErrors());
    }

    private SourceFile findSource(String className) {
        return sourceFileFinder(className).findSourceFile();
    }

    private SourceFileFinder sourceFileFinder(String className) {
        return new SourceFileFinder(className);
    }
}
