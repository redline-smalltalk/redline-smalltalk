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

public class SourceFilesFinder {

	private final String paths;

	public SourceFilesFinder(String paths) {
		this.paths = paths;
	}

	public List<SourceFile> findSourceFiles() {
		String[] parts = paths.split(" ");
		List<SourceFile> sourceFiles = new ArrayList<SourceFile>();
		for (String path : parts)
			addSourceFiles(path, sourceFiles);
		return sourceFiles;
	}

	private void addSourceFiles(String path, List<SourceFile> sourceFiles) {
		if (isPathToClass(path))
			addClassInPath(path, sourceFiles);
		else
			addClassesInPath(path, sourceFiles);
	}

	private void addClassesInPath(String path, List<SourceFile> sourceFiles) {
		String classesPath = stripWildcard(path);
		for (String file : SourceFileFinder.findInPackage(classesPath))
			sourceFiles.add(new SourceFile(new File(file)));
	}

	private String stripWildcard(String path) {
		if (path.endsWith("*"))
			return path.substring(0, path.length() - 2);
		return path;
	}

	private void addClassInPath(String path, List<SourceFile> sourceFiles) {
		SourceFile sourceFile = sourceFileFinder(path).findSourceFile();
		if (sourceFile != null)
			sourceFiles.add(sourceFile);
		else
			System.err.println("WARNING: Didn't find file '" + path + "'.");
	}

	private SourceFileFinder sourceFileFinder(String className) {
		return new SourceFileFinder(className);
	}

	private boolean isPathToClass(String path) {
		String className = path;
		int index = path.lastIndexOf('.');
		if (index != -1)
			className = path.substring(index + 1);
		return Character.isUpperCase(className.charAt(0));
	}
}
