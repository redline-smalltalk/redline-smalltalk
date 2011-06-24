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

import java.io.File;

public class SourceFile extends File {

	protected SourceFile(String pathname) {
		super(pathname);
	}

	public static SourceFile on(String pathname) {
		return new SourceFile(pathname);
	}

	public int startingLineNumber() {
		return 0;
	}

	public String contents() {
		return sourceFileReader().read(this);
	}

	private SourceFileReader sourceFileReader() {
		return new SourceFileReader();
	}

	public String extension() {
		String path = getName();
		return path.substring(path.lastIndexOf('.') + 1);
	}

	public String packageName() {
		String path = this.toString();
		path = path.substring(0, path.lastIndexOf(className()));
		return path.endsWith(File.separator) ? path.substring(0, path.length() - 1) : path;
	}

	public String className() {
		String name = getName();
		return name.substring(0, name.lastIndexOf("."));
	}
}
