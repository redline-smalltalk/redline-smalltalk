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

	public SourceFile(File file) {
		super(file.getAbsolutePath());
	}

	public String contents() {
		return sourceFileReader().read(this);
	}

	private SourceFileReader sourceFileReader() {
		return new SourceFileReader();
	}

	public String shortName() {
		String name = name();
		// assumes we have an file extension.
		return name.substring(name.lastIndexOf(File.separatorChar) + 1, name.lastIndexOf('.'));
	}

	public String packageName() {
		String name = name();
		int index = name.lastIndexOf(File.separatorChar);
		if (index == -1)
			return "";
		return name.substring(0, index);
	}

	private String name() {
		String name = toString();
		String userDir = userDir();
		if (name.startsWith(userDir))
			return name.substring(userDir.length() + 1);
		return name;
	}

	private String userDir() {
		return System.getProperty("user.dir");
	}
}
