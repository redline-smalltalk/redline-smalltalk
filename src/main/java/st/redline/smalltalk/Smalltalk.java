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

import java.io.File;
import java.io.PrintWriter;

public class Smalltalk {

	public static final String CURRENT_FILE = "CURRENT_FILE";

	private final Environment environment;

	private File currentFile;

	public static Smalltalk with(Environment environment) {
		if (environment == null)
			throw new MissingArgumentException();
		return new Smalltalk(environment);
	}

	private Smalltalk(Environment environment) {
		this.environment = environment;
	}

	public void eval(String source) {
	}

	public void eval(File file) {
		if (isNotFile(file))
			return;
		currentFile = file;
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
		return "";
	}

	private boolean isNotFile(File file) {
		return file == null || !file.exists() || !file.isFile();
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
}
