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

public class ClassFinder {

	protected static final String SMALLTALK_SOURCE_EXTENSION = ".st";

	private final Smalltalk smalltalk;

	public ClassFinder(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public Class<?> find(String name) throws ClassNotFoundException {
		System.out.println("find() " + name);
		Class<?> cls = findClassInDefaultPackage(name);
		if (cls != null)
			return cls;
		cls = loadClassFromSource(name);
		if (cls != null)
			return cls;
		return null;
	}

	private Class<?> findClassInDefaultPackage(String name) {
		if (!name.startsWith(Smalltalk.REDLINE_PACKAGE)) {
			return findClassIn(name, Smalltalk.REDLINE_PACKAGE);
		}
		return null;
	}

	private Class<?> findClassIn(String name, String inPackage) {
		System.out.println("findClassIn() " + name + " in " + inPackage);
		try {
			return loadClass(inPackage + "." + name);
		} catch (ClassNotFoundException e) {
			return null;
		}
	}

	private Class<?> loadClass(String name) throws ClassNotFoundException {
		return smalltalk.loadClass(name);
	}

	private Class<?> loadClassFromSource(String name) throws ClassNotFoundException {
		System.out.println("loadClassFromSource() " + name);
		File source = findClassSource(name);
		if (source != null) {
			smalltalk.evaluate(source);
			return loadClass(name);
		}
		return null;
	}

	private File findClassSource(String name) {
		String filename = makeSourceFilename(name);
		return findFileInSourcePath(filename);
	}

	private String makeSourceFilename(String name) {
		return name.replaceAll("\\.", Matcher.quoteReplacement(File.separator)) + SMALLTALK_SOURCE_EXTENSION;
	}

	private File findFileInSourcePath(String filename) {
		System.out.println("findFileInSourcePath() " + filename);
		for (String path : sourcePaths()) {
			File file = findFile(filename, path);
			if (file != null)
				return file;
		}
		return null;
	}

	public List<String> sourcePaths() {
		List<String> paths = new ArrayList<String>();
		paths.add("src/main/smalltalk");
		return paths;
	}

	private File findFile(String filename, String path) {
		System.out.println("findFile() " + filename + " in " + path);
		File file = new File(path + File.separator + filename);
		if (file.exists())
			return file;
		return null;
	}
}
