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
