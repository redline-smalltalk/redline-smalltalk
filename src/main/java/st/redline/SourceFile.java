/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
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
		name = name.substring(0, index);
		for (String path : SourceFileFinder.sourceFilePaths())
			if (name.startsWith(path))
				return (path.length() == name.length()) ? "" : name.substring(path.length() + 1);
		return name;
	}

	private String name() {
		String name = toString();
		String userDir = userDir();
		if (name.startsWith(userDir))
			name = name.substring(userDir.length() + 1);
		if (name.startsWith("rt"))
			return name.substring(3);
		return name;
	}

	private String userDir() {
		return System.getProperty("user.dir");
	}
}
