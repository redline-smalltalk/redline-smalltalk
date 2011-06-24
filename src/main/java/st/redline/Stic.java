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

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline;

public class Stic {

	protected final Smalltalk smalltalk = new Smalltalk();

	private Smalltalk smalltalk() {
		return smalltalk;
	}

	public static void main(String[] args) {
		Stic.execute(files(args));
	}

	private static void execute(SourceFiles sourceFiles) {
		new Stic().evaluate(sourceFiles);
	}

	private void evaluate(SourceFiles sourceFiles) {
		for (SourceFile sourceFile : sourceFiles)
			evaluate(sourceFile);
	}

	private void evaluate(SourceFile sourceFile) {
		smalltalk().evaluate(sourceFile);
	}

	private static SourceFiles files(String[] args) {
		return SourceFiles.with(sourceFile(args[0]));
	}

	private static SourceFile sourceFile(String filename) {
		return SourceFile.on(filename);
	}
}
