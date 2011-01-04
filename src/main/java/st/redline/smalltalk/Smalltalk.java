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

/**
 * Provides an entry point for Redline Smalltalk.
 */
public class Smalltalk extends ClassLoader {

	public static final String CURRENT_FILE = "CURRENT_FILE";
	public static final String INTERPRETER = "INTERPRETER";
	public static final String GENERATOR = "GENERATOR";
	public static final String FILE_READER = "FILE_READER";

	private final Environment environment;
	private final ClassLoader parentClassLoader;
	private SourceFile currentFile;

	public static Smalltalk with(Environment environment) {
		if (environment == null)
			throw new MissingArgumentException();
		return new Smalltalk(environment, currentClassLoader());
	}

	private static ClassLoader currentClassLoader() {
		return Thread.currentThread().getContextClassLoader();
	}

	private Smalltalk(Environment environment, ClassLoader classLoader) {
		this.environment = environment;
		this.parentClassLoader = classLoader;
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

	public CommandLine commandLine() {
		return environment.commandLine();
	}

	public SourceFile currentFile() {
		return currentFile;
	}

	public String userPath() {
		return System.getProperty("user.dir");
	}
}
