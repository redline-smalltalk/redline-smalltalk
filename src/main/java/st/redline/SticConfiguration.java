/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline;

import st.redline.classloader.*;
import st.redline.compiler.*;
import st.redline.compiler.Compiler;

import java.io.File;

public class SticConfiguration {

    private final String[] args;
    private final JvmArguments jvmArguments;

    public SticConfiguration(String[] args) {
        this.args = args;
        jvmArguments = new JvmArguments();
    }

    public ClassLoader classLoader() throws Exception {
        ClassBuilder classBuilder = classBuilder();
        Bootstrapper bootstrapper = bootstrapper();
        boolean traceClassLoading = isTraceClassLoading();
        SmalltalkClassLoader classLoader = new SmalltalkClassLoader(classBuilder, bootstrapper, traceClassLoading);
        classLoader.bootstrap();
        return classLoader;
    }

    public Bootstrapper bootstrapper() {
        SourceFinder sourceFinder = sourceFinder();
        return new Bootstrapper(sourceFinder);
    }

    public ClassBuilder classBuilder() {
        SourceFinder sourceFinder = sourceFinder();
        Compiler compiler = compiler();
        return new ClassBuilder(sourceFinder, compiler);
    }

    public SourceFinder sourceFinder() {
        String[] classPaths = classPaths();
        SourceFactory sourceFactory = sourceFactory();
        return new SourceFinder(sourceFactory, classPaths);
    }

    public Compiler compiler() {
        Preprocessor preprocessor = preprocessor();
        Parser parser = parser();
        AnalyserFactory analyserFactory = analyserFactory();
        return new Compiler(preprocessor, parser, analyserFactory);
    }

    public Preprocessor preprocessor() {
        boolean tracePreprocessor = isTracePreprocessor();
        return new Preprocessor(tracePreprocessor);
    }

    public Parser parser() {
        boolean ignoreParseErrors = ignoreParseErrors();
        return new Parser(ignoreParseErrors);
    }

    public boolean ignoreParseErrors() {
        return jvmArguments.ignoreParseErrors();
    }

    public AnalyserFactory analyserFactory() {
        BytecodeWriterFactory bytecodeWriterFactory = bytecodeWriterFactory();
        boolean traceAnalysis = isTraceAnalysis();
        return new AnalyserFactory(bytecodeWriterFactory, traceAnalysis);
    }

    public BytecodeWriterFactory bytecodeWriterFactory() {
        boolean traceBytecode = isTraceBytecodeWriting();
        return new BytecodeWriterFactory(traceBytecode);
    }

    public SourceFactory sourceFactory() {
        return new SourceFactory();
    }

    public String[] classPaths() {
        return classPath().split(File.pathSeparator);
    }

    private String classPath() {
        return System.getProperty("java.class.path");
    }

    public String scriptName() {
        return argument(0);
    }

    private String argument(int index) {
        if (args != null && args.length > index)
            return args[index];
        return "";
    }

    public boolean isTraceClassLoading() {
        return jvmArguments.isTraceClassLoading();
    }

    public boolean isTracePreprocessor() {
        return jvmArguments.isTracePreprocessor();
    }

    public boolean isTraceAnalysis() {
        return jvmArguments.isTraceAnalysis();
    }

    public boolean isTraceBytecodeWriting() {
        return jvmArguments.isTraceBytecodeWriting();
    }
}
