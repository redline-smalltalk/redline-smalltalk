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
package st.redline.bootstrap;

import st.redline.RMethod;
import st.redline.RObject;
import st.redline.SourceFile;

import java.io.File;

public class CompileMethod extends RMethod {

	private final boolean isClassMethod;

	public CompileMethod(boolean isClassMethod) {
		this.isClassMethod = isClassMethod;
	}

	public RObject applyToWith(RObject receiver, RObject classMethodWasFoundIn, RObject selector, RObject lineNumber, RObject methodSource) {
		System.out.println("CompileMethod: " + methodSource.primitiveValue().toString() + " into " + receiver.name());
		String selectorValue = selector.primitiveValue().toString();
		RMethod method = (RMethod) smalltalk().evaluate(sourceFileFor(receiver, selectorValue, methodSource, lineNumber));
		if (isClassMethod)
			receiver.cls().methodAtPut(selectorValue, method);
		else
			receiver.methodAtPut(selectorValue, method);
		return receiver;
	}

	private SourceFile sourceFileFor(RObject receiver, String selector, RObject methodSource, RObject lineNumber) {
		return sourceFileFor(receiver, selector, methodSource.primitiveValue().toString(), lineNumber.primitiveValue().toString());
	}

	private SourceFile sourceFileFor(RObject receiver, String selector, String source, String lineNumber) {
		return sourceFileFor(receiver, selector, source, Integer.parseInt(lineNumber));
	}

	private SourceFile sourceFileFor(RObject receiver, String selector, String source, int lineNumber) {
		return new MethodOnlySourceFile(receiver, selector, source, lineNumber);
	}

	private class MethodOnlySourceFile extends SourceFile {

		private final String selector;
		private final String source;
		private final int lineNumber;

		public MethodOnlySourceFile(RObject receiver, String selector, String source, int lineNumber) {
			super(receiver.primitiveSourceFile());
			this.selector = selector;
			this.source = source;
			this.lineNumber = lineNumber;
		}

		public String contents() {
			return source;
		}

		public int startingLineNumber() {
			return lineNumber;
		}

		public String packageName() {
			String path = this.toString();
			path = path.substring(0, path.lastIndexOf(super.className()));
			return path.endsWith(File.separator) ? path.substring(0, path.length() - 1) : path;
		}

		public String className() {
			return super.className() + "$" + selector;
		}
	}
}
