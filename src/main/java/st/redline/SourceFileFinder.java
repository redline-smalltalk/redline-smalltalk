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
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

public class SourceFileFinder {

	private final String sourceFileName;
	private final String className;

	public SourceFileFinder(String className) {
		this.className = className;
		this.sourceFileName = makeSourceFileName(className);
	}

	public SourceFile findSourceFile() {
		return findSourceFile(sourceFileName);
	}

	private String makeSourceFileName(String className) {
		return className.replaceAll("\\.", Matcher.quoteReplacement(File.separator)) + ".st";
	}

	private SourceFile findSourceFile(String sourceFileName) {
		SourceFile sourceFile;
		for (String sourceFilePath : sourceFilePaths())
			if ((sourceFile = findSourceFile(sourceFilePath, sourceFileName)) != null)
				return sourceFile;
		return null;
	}

	private SourceFile findSourceFile(String sourceFilePath, String sourceFileName) {
		File file = new File(sourceFilePath + File.separator + sourceFileName);
		if (file.exists())
			return new SourceFile(file);
		return null;
	}

	protected static List<String> sourceFilePaths() {
		List<String> sourceFilePaths = new ArrayList<String>();
		sourceFilePaths.add(System.getProperty("user.dir"));
		sourceFilePaths.add("src/main/smalltalk");
		sourceFilePaths.add("src/test/smalltalk");
		return sourceFilePaths;
	}
}
