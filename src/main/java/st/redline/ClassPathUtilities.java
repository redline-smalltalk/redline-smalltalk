/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.File;

public class ClassPathUtilities {
    public static String filenameToClassName(String filename) {
        return filename.substring(filename.lastIndexOf(File.separatorChar) + 1, filename.lastIndexOf('.'));
    }

    public static String filenameWithExtensionToPackageName(String fileName) {
        return filenameToPackageName(fileName).substring(0, fileName.lastIndexOf(File.separator));
    }

    public static String filenameToPackageName(String filename) {
		int index = filename.lastIndexOf(File.separatorChar);
		if (index == -1)
			return "";
		filename = filename.substring(0, index);
		for (String path : SourceFileFinder.sourceFilePaths())
			if (filename.startsWith(path))
				return (path.length() == filename.length()) ? "" : filename.substring(path.length() + 1).replace(File.separator, ".");
		return filename.replace(File.separator, ".");
    }

    public static String classNameToFullyQualifiedClassName(String packageName, String className) {
        return (packageName.length() > 0 ? packageName + "." + className : className).replace(".", "/");
    }

    public static String classNameToFileName(String sourcePath, String className) {
        return sourcePath + File.separator + className.replace(".", File.separator) + ".st";
    }
}
