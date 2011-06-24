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

import java.io.*;

public class SourceFileReader {

	private static final int LINE_BUFFER_INITAL_CAPACITY = 1024;

	private String filename;
	private String fullPath;

	public String read(File file) {
		Reader reader = createReaderOn(file);
		try {
			return readContentsWith(reader);
		} finally {
			close(reader);
		}
	}

	private String readContentsWith(Reader reader) {
		String line;
		StringBuffer lineBuffer = new StringBuffer(LINE_BUFFER_INITAL_CAPACITY);
		BufferedReader lineReader = new BufferedReader(reader);
		PreProcessor preProcessor = new PreProcessor(filename, fullPath);
		int number = 0;
		try {
			while ((line = lineReader.readLine()) != null)
				lineBuffer.append(preProcessor.process(line, number++));
			preProcessor.process(lineBuffer);
		} catch (IOException e) {
			throw new IllegalStateException("Error reading file: " + e.getMessage(), e);
		}
		return lineBuffer.toString();
	}

	private Reader createReaderOn(File file) {
		try {
			fullPath = file.getPath();
			filename = nameWithoutExtension(file, ".st");
			return new java.io.FileReader(file);
		} catch (FileNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	private String nameWithoutExtension(File file, String extension) {
		String name = file.getName();
		int offset = name.lastIndexOf(extension);
		if (offset > 0)
			return name.substring(0, offset);
		return name;
	}

	private void close(Reader reader) {
		try {
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private class PreProcessor {

		private static final String METHOD_END = "`. ";
		private final String filename;
		private final String fullPath;
		private boolean processedMethods = false;

		public PreProcessor(String filename, String fullPath) {
			this.filename = filename;
			this.fullPath = fullPath;
		}

		public String process(String line, int number) {
			char firstChar = line.length() > 0 ? line.charAt(0) : 0;
			if (firstChar == '+')
				line = processClassMethodLine(line, number);
			else if (firstChar == '-')
				line = processInstanceMethodLine(line, number);
			return line + "\n";
		}

		private String processInstanceMethodLine(String line, int number) {
			return processLine("compileInstanceMethod:", "def", line, number);
		}

		private String processClassMethodLine(String line, int number) {
			return processLine("compileClassMethod:", "cdef", line, number);
		}

		private String processLine(String type, String prefix, String line, int number) {
			String processed = (processedMethods ? METHOD_END : (filename + " primitiveSourceFile: '" + fullPath + "'. " ))
					+ filename + " " + type + " #" + selectorFrom(line) + " at: '" + number + "' fromSource: `" + prefix + line.substring(1);
			// TODO.jcl pass at: param above as number, not string.
			processedMethods = true;
			return processed;
		}

		private String selectorFrom(String line) {
			System.out.println("selectorFrom() " + line);
			int begin = 0;
			while (begin < line.length() && !Character.isLetterOrDigit(line.charAt(begin)))
				begin++;
			int end = begin;
			while (end < line.length() && Character.isLetterOrDigit(line.charAt(end)))
				end++;
			return line.substring(begin, end);
		}

		public StringBuffer process(StringBuffer lineBuffer) {
			if (processedMethods)
				lineBuffer.append(METHOD_END);
			return lineBuffer;
		}
	}
}
