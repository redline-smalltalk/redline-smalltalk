/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.*;

public class SourceFileReader {

	private static final int LINE_BUFFER_INITAL_CAPACITY = 1024;

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
		try {
			while ((line = lineReader.readLine()) != null)
				lineBuffer.append(line).append('\n');
		} catch (IOException e) {
			throw RedlineException.withCause(e);
		}
		return lineBuffer.toString();
	}

	private Reader createReaderOn(File file) {
		try {
			return new java.io.FileReader(file);
		} catch (FileNotFoundException e) {
			throw RedlineException.withCause(e);
		}
	}

	private void close(Reader reader) {
		try {
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
