/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// TODO.JCL - make this classloader delegate to another so we can replace the delegate at runtime to reload all CLASSES on fly.

import java.util.Hashtable;
import java.util.List;
import java.util.Map;

public class SmalltalkClassLoader extends ClassLoader {

	public static Map<String, Object> BLOCKS_TO_BE_COMPILED = new Hashtable<String, Object>();
	private final CommandLine commandLine;

	public SmalltalkClassLoader(java.lang.ClassLoader classLoader, CommandLine commandLine) {
		super(classLoader);
		this.commandLine = commandLine;
	}

	public static SmalltalkClassLoader instance() {
		return (SmalltalkClassLoader) Thread.currentThread().getContextClassLoader();
	}

	protected void bootstrap() {
		loadPrimObjectMetaclass().bootstrap();
	}

	private PrimObjectMetaclass loadPrimObjectMetaclass() {
		try {
			return ((PrimObjectMetaclass) loadClass("st.redline.PrimObjectMetaclass").newInstance());
		} catch (Exception e) {
			throw RedlineException.withCause(e);
		}
	}

	public CommandLine commandLine() {
		return commandLine;
	}

	public Class findClass(String className) throws ClassNotFoundException {
		return findClass0(className);
	}

	public Class findClass0(String className) throws ClassNotFoundException {
//		System.out.println("findClass() " + className);
		SourceFile sourceFile = findSource(className);
		if (sourceFile == null)
			return tryFindSystemClass(className);
		return classFrom(sourceFile);
	}

	private Class tryFindSystemClass(String className) {
		try {
			return findSystemClass(className);
		} catch (ClassNotFoundException e) {
			throw RedlineException.withCause(e);
		}
	}

	private Class classFrom(SourceFile sourceFile) {
//		System.out.println("classFrom() " + sourceFile);
		byte[] classBytes = compile(sourceFile);
		return defineClass(null, classBytes, 0, classBytes.length);
	}

	public Class defineClass(byte[] classBytes) {
		return defineClass(null, classBytes, 0, classBytes.length);
	}

	private byte[] compile(SourceFile sourceFile) {
		return createCompiler(sourceFile).compile();
	}

	private Compiler createCompiler(SourceFile sourceFile) {
		return new Compiler(sourceFile, commandLine.verboseRequested(), commandLine.ignoreCompilerErrors());
	}

	private SourceFile findSource(String className) {
		return sourceFileFinder(className).findSourceFile();
	}

	private SourceFileFinder sourceFileFinder(String className) {
		return new SourceFileFinder(className);
	}

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
