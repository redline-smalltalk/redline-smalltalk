/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
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
