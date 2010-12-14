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
*/
package st.redline.testsupport;

import java.io.*;
import java.util.Locale;

/**
 * Captures a PrintStreams contents on a String.
 * use contents() to retrieve the PrintStreams contents.
 */
public class StringPrintStream extends PrintStream {

	public static final String LINE_SEPARATOR = System.getProperty("line.separator");

	private final StringBuffer contents = new StringBuffer();

	public StringPrintStream(OutputStream out) {
		super(out);
	}

	public String contents() {
		return contents.toString().trim();
	}

	public void flush() {
	}

	public void close() {
	}

	public void write(int b) {
		contents.append((char) b);
	}

	public void write(byte buf[], int off, int len) {
		for (int i = off; i < len; i++)
			write(buf[i]);
	}

	public void print(boolean b) {
		contents.append(b);
	}

	public void print(char c) {
		contents.append(c);
	}

	public void print(int i) {
		contents.append(i);
	}

	public void print(long l) {
		contents.append(l);
	}

	public void print(float f) {
		contents.append(f);
	}

	public void print(double d) {
		contents.append(d);
	}

	public void print(char[] s) {
		contents.append(s);
	}

	public void print(String s) {
		contents.append(s);
	}

	public void print(Object obj) {
		contents.append(obj);
	}

	public void println() {
		contents.append(LINE_SEPARATOR);
	}

	public void println(boolean x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(char x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(int x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(long x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(float x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(double x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(char[] x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(String x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public void println(Object x) {
		contents.append(x).append(LINE_SEPARATOR);
	}

	public StringPrintStream printf(String format, Object... args) {
		throw new IllegalStateException("Not implemented.");
	}

	public StringPrintStream printf(Locale l, String format, Object... args) {
		throw new IllegalStateException("Not implemented.");
	}

	public StringPrintStream format(String format, Object... args) {
		throw new IllegalStateException("Not implemented.");
	}

	public StringPrintStream format(Locale l, String format, Object... args) {
		throw new IllegalStateException("Not implemented.");
	}

	public StringPrintStream append(CharSequence csq) {
		contents.append(csq);
		return this;
	}

	public PrintStream append(CharSequence csq, int start, int end) {
		contents.append(csq, start, end);
		return this;
	}

	public StringPrintStream append(char c) {
		contents.append(c);
		return this;
	}
}
