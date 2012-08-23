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

    public List<SourceFile> findSources(String paths) {
        return findSources(paths, true);
    }

    public List<SourceFile> findSources(String paths, boolean warnings) {
        return sourceFilesFinder(paths, warnings).findSourceFiles();
    }

    private SourceFilesFinder sourceFilesFinder(String paths, boolean warnings) {
        return new SourceFilesFinder(paths, warnings, classLoader());
    }

    public void registerBlockToBeCompiled(Object block, String name) {
        if (BLOCKS_TO_BE_COMPILED.containsKey(name))
            throw new IllegalStateException("Block to be compiled registered twice: " + name);
        BLOCKS_TO_BE_COMPILED.put(name, block);
    }
}
