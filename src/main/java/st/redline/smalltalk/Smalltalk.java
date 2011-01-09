/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package st.redline.smalltalk;

import st.redline.smalltalk.interpreter.Generator;
import st.redline.smalltalk.interpreter.Interpreter;

import java.io.File;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

/**
 * Provides an entry point for Redline Smalltalk.
 */
public class Smalltalk extends ClassLoader {

	public static final String SMALLTALK_SOURCE_EXTENSION = ".st";
	public static final String REDLINE_PACKAGE = "st.redline.smalltalk";
	public static final String CURRENT_FILE = "CURRENT_FILE";
	public static final String INTERPRETER = "INTERPRETER";
	public static final String GENERATOR = "GENERATOR";
	public static final String FILE_READER = "FILE_READER";

	private final Environment environment;
	private final Map<String, ProtoObject> cachedObjects;

	private SourceFile currentFile;

	public static Smalltalk with(Environment environment) {
		if (environment == null)
			throw new MissingArgumentException();
		return new Smalltalk(environment, currentClassLoader());
	}

	private static ClassLoader currentClassLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	private Smalltalk(Environment environment, ClassLoader parentClassLoader) {
		super(parentClassLoader);
		this.environment = environment;
		this.cachedObjects = new HashMap<String, ProtoObject>();
		initialize();
	}

	private void initialize() {
		if (fileReader() == null)
			fileReader(new FileReader());
		if (interpreter() == null)
			interpreter(new Interpreter());
		if (generator() == null)
			generator(new Generator());
		Thread.currentThread().setContextClassLoader(this);
	}

	public Environment environment() {
		return environment;
	}

	public void eval(String source) {
		interpreter().interpretUsing(source, this);
	}

	public void eval(File file) {
		currentFile = new SourceFile(file.getAbsolutePath(), this);
		evalFile();
	}

	private void evalFile() {
		trackFile();
		try {
			eval(fileContents());
		} finally {
			untrackFile();
		}
	}

	private String fileContents() {
		return currentFile.contents();
	}

	public Generator generator() {
		return (Generator) environment.get(GENERATOR);
	}

	private void generator(Generator generator) {
		environment.put(GENERATOR, generator);
	}

	public Interpreter interpreter() {
		return (Interpreter) environment.get(INTERPRETER);
	}

	private void interpreter(Interpreter interpreter) {
		environment.put(INTERPRETER, interpreter);
	}

	public FileReader fileReader() {
		return (FileReader) environment.get(FILE_READER);
	}

	private void fileReader(FileReader fileReader) {
		environment.put(FILE_READER, fileReader);
	}

	private void untrackFile() {
		environment.remove(CURRENT_FILE);
	}

	private void trackFile() {
		environment.put(CURRENT_FILE, currentFile);
	}

	public PrintWriter standardOutput() {
		return environment.standardOutput();
	}

	public PrintWriter errorOutput() {
		return environment.errorOutput();
	}

	public CommandLine commandLine() {
		return environment.commandLine();
	}

	public SourceFile currentFile() {
		return currentFile;
	}

	public String userPath() {
		return System.getProperty("user.dir");
	}

	public List<String> sourcePaths() {
		return commandLine().sourcePaths();
	}

	public Class defineClass(byte[] classBytes) {
		return defineClass(null, classBytes, 0, classBytes.length);
	}

	public ProtoObject basicAt(String className) {
		ProtoObject cached = cachedObject(className);
		if (cached != null)
			return cached;
		if (resolveClassObject(className))
			return cachedObject(className);
		// todo - this should return nil.
		return null;
	}

	private boolean resolveClassObject(String name) {
		try {
			loadClass(name).newInstance();
			return true;
		} catch (Exception e) {
			e.printStackTrace(errorOutput());
			errorOutput().flush();
		}
		return false;
	}

	protected Class<?> findClass(String name) throws ClassNotFoundException {
		Class<?> cls = findClassInDefaultPackage(name);
		if (cls != null)
			return cls;
		cls = loadClassFromSource(name);
		if (cls != null)
			return cls;
		return super.findClass(name);
	}

	private Class<?> loadClassFromSource(String name) throws ClassNotFoundException {
		File source = findClassSource(name);
		if (source != null) {
			eval(source);
			return loadClass(name);
		}
		return null;
	}

	private File findClassSource(String name) {
		String filename = makeSourceFilename(name);
		return findFileInSourcePath(filename);
	}

	private File findFileInSourcePath(String filename) {
		for (String path : sourcePaths()) {
			File file = findFile(filename, path);
			if (file != null)
				return file;
		}
		return null;
	}

	private File findFile(String filename, String path) {
		File file = new File(path + File.separator + filename);
		if (file.exists())
			return file;
		return null;
	}

	private String makeSourceFilename(String name) {
		return name.replaceAll("\\.", Matcher.quoteReplacement(File.separator)) + SMALLTALK_SOURCE_EXTENSION;
	}

	private Class<?> findClassInDefaultPackage(String name) {
		if (!name.startsWith(REDLINE_PACKAGE)) {
			return findClassIn(name, REDLINE_PACKAGE);
		}
		return null;
	}

	private Class<?> findClassIn(String name, String inPackage) {
		try {
			return loadClass(inPackage + "." + name);
		} catch (ClassNotFoundException e) {
			return null;
		}
	}

	private ProtoObject cachedObject(String name) {
		return cachedObjects.get(name);
	}
}
