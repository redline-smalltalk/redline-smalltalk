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

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import st.redline.interpreter.*;

import java.io.File;

public class Interpreter {

	private final Smalltalk smalltalk;

	private SourceFile sourceFile;

	public Interpreter(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public Object interpret(SourceFile sourceFile) {
		this.sourceFile = sourceFile;
		return interpret(sourceFile.contents());
	}

	protected Object interpret(String source) {
		return execute(parse(source));
	}

	private Object execute(Program program) {
		return smalltalk.defineClass(analyse(program).classBytes(), true);
	}

	private Analyser analyse(Program program) {
		Analyser analyser = analyser();
		program.accept(analyser);
		return analyser;
	}

	private Analyser analyser() {
		return new Analyser(className(), packageName(), sourceFileExtension(), sourceFile.startingLineNumber());
	}

	private String sourceFileExtension() {
		return sourceFile.extension();
	}

	private String packageName() {
		return sourceFile.packageName();
	}

	private String className() {
		return sourceFile.className();
	}

	private Program parse(String sourceCode) {
		SmalltalkLexer smalltalkLexer = lexorOn(sourceCode);
		SmalltalkParser smalltalkParser = parserUsing(smalltalkLexer);
		try {
			return smalltalkParser.program();
		} catch (RecognitionException e) {
			throw new IllegalStateException(e);
		}
	}

	private SmalltalkParser parserUsing(SmalltalkLexer smalltalkLexer) {
		return new SmalltalkParser(new CommonTokenStream(smalltalkLexer));
	}

	private SmalltalkLexer lexorOn(String sourceCode) {
		return new SmalltalkLexer(new ANTLRStringStream(sourceCode));
	}
}
