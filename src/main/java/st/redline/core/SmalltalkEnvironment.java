/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import java.lang.*;
import java.net.MalformedURLException;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

public class SmalltalkEnvironment {

    public static Map<String, Object> BLOCKS_TO_BE_COMPILED = new Hashtable<String, Object>();
    public static Map<ClassLoader, SmalltalkEnvironment> ENVIRONMENTS = new Hashtable<ClassLoader, SmalltalkEnvironment>();
    private final CommandLine commandLine;

    public SmalltalkEnvironment(CommandLine commandLine) throws MalformedURLException {
        this.commandLine = commandLine;
        ClassLoader classLoader = createNewClassLoader();
        Thread.currentThread().setContextClassLoader(classLoader);
        ENVIRONMENTS.put(classLoader(), this);
    }

    private ClassLoader createNewClassLoader() throws MalformedURLException {
        return new RedlineClassLoaderBuilder(new ClassLoaderCreatorImpl()).build(classLoader(), commandLine);
    }

    public static SmalltalkEnvironment instance() {
        return ENVIRONMENTS.get(classLoader());
    }

    public static ExposedClassLoader exposedClassLoader() {
        return (ExposedClassLoader) classLoader();
    }

    public static ClassLoader classLoader() {
        return Thread.currentThread().getContextClassLoader();
    }

    protected void bootstrap() {
        loadPrimObjectMetaclass().bootstrap();
    }

    private PrimObjectMetaclass loadPrimObjectMetaclass() {
        try {
            return ((PrimObjectMetaclass) loadClass("st.redline.core.PrimObjectMetaclass").newInstance());
        } catch (Exception e) {
            throw RedlineException.withCause(e);
        }
    }

    private Class loadClass(String className) throws ClassNotFoundException {
        return classLoader().loadClass(className);
    }

    public CommandLine commandLine() {
        return commandLine;
    }

//    public Class findClass(String className) throws ClassNotFoundException {
//        return findClass0(className);
//    }
//
//    public Class findClass0(String className) throws ClassNotFoundException {
//        SourceFile sourceFile = findSource(className);
//        if (sourceFile == null)
//            return tryFindSystemClass(className);
//        return classFrom(sourceFile);
//    }

//    private Class tryFindSystemClass(String className) {
//        try {
//            return findSystemClass(className);
//        } catch (ClassNotFoundException e) {
//            throw RedlineException.withCause(e);
//        }
//    }

//    public Class classFrom(SourceFile sourceFile) {
//        byte[] classBytes = compile(sourceFile);
//        return classLoader().defineClass(null, classBytes, 0, classBytes.length);
//    }
//
//    public Class defineClass(byte[] classBytes) {
//        return defineClass(null, classBytes, 0, classBytes.length);
//    }
//
//    public Class defineClass(String name, byte[] classBytes) {
//        return defineClass(name, classBytes, 0, classBytes.length);
//    }
//
//    private byte[] compile(SourceFile sourceFile) {
//        return createCompiler(sourceFile).compile();
//    }
//
//    private Kompiler createCompiler(SourceFile sourceFile) {
//        return new Kompiler(sourceFile, commandLine.verboseRequested(), commandLine.ignoreCompilerErrors());
//    }
//
//    private SourceFile findSource(String className) {
//        return sourceFileFinder(className).findSourceFile();
//    }
//
//    private SourceFileFinder sourceFileFinder(String className) {
//        return new SourceFileFinder(className);
//    }
//
    public List<SourceFile> findSources(String paths) {
        return sourceFilesFinder(paths).findSourceFiles();
    }

    private SourceFilesFinder sourceFilesFinder(String paths) {
        return new SourceFilesFinder(paths);
    }

    public void registerBlockToBeCompiled(Object block, String name) {
        if (BLOCKS_TO_BE_COMPILED.containsKey(name))
            throw new IllegalStateException("Block to be compiled registered twice: " + name);
        BLOCKS_TO_BE_COMPILED.put(name, block);
    }
}
