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
package st.redline.smalltalk.interpreter;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import st.redline.smalltalk.Smalltalk;

import java.io.PrintStream;
import java.io.PrintWriter;

public class Interpreter {

	private Smalltalk smalltalk;
	private Analyser analyser;
	private Program program;
	private Class programClass;

	public void interpretUsing(String sourceCode, Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
		this.analyser = analyser();
		interpret(parse(sourceCode));
	}

	private Analyser analyser() {
		return new Analyser(smalltalk, generator());
	}

	private Generator generator() {
		return smalltalk.generator();
	}

	private void interpret(Program program) {
		if (program == null)
			return;
		this.program = program;
		compileProgram();
		loadProgram();
		runProgram();
	}

	private void runProgram() {
		try {
			if (programClass != null)
				programClass.newInstance();
		} catch (InstantiationException e) {
			e.printStackTrace(errorOutput());
		} catch (IllegalAccessException e) {
			e.printStackTrace(errorOutput());
		}
	}

	private PrintWriter errorOutput() {
		return smalltalk.errorOutput();
	}

	private void loadProgram() {
		programClass = smalltalk.defineClass(analyser.result());
	}

	private void compileProgram() {
		program.accept(analyser);
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
