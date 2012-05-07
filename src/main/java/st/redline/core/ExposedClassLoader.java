/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

// NOTE: This classLoader exposes methods that are usually protected in the classLoader hierarchy.
// This violates some contracts/principles.

public class ExposedClassLoader extends ClassLoader {

    private final CommandLine commandLine;

    public ExposedClassLoader(ClassLoader parent, CommandLine commandLine) {
        super(parent);
        this.commandLine = commandLine;
    }

    public Class defineClass(byte[] classBytes) {
        return defineClass(null, classBytes, 0, classBytes.length);
    }

    public Class classFrom(SourceFile sourceFile) {
        byte[] classBytes = compile(sourceFile);
        return defineClass(null, classBytes, 0, classBytes.length);
    }

    private byte[] compile(SourceFile sourceFile) {
        return createCompiler(sourceFile).compile();
    }

    private Kompiler createCompiler(SourceFile sourceFile) {
        return new Kompiler(sourceFile, commandLine.verboseRequested(), commandLine.ignoreCompilerErrors());
    }
}
