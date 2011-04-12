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

import st.redline.smalltalk.Smalltalk;
import st.redline.smalltalk.SourceFile;

import java.util.*;

public class Analyser implements NodeVisitor {

	private final Smalltalk smalltalk;
	private final Generator generator;
	private final List<byte[]> methodClasses;

    public Analyser(Smalltalk smalltalk, Generator generator) {
		this.smalltalk = smalltalk;
		this.generator = generator;
		this.methodClasses = new ArrayList<byte[]>();
		generator.initialize();
	}

	public byte[] classResult() {
		return generator.classBytes();
	}

	public List<byte[]> methodClassResults() {
		return methodClasses;
	}

	public void visit(Program program) {
	}

	public void visit(Temporaries temporaries) {
	}

	public void visit(Temporary temporary, String value, int line) {
	}

	public void visit(VariableName variableName, String value, int line) {
	}

	public void visit(Statements statements) {
	}

	public void visit(AnswerExpression answerExpression) {
	}

	public void visit(Methods methods) {
	}

	public void visit(InstanceMethod instanceMethod) {
	}

	public void visit(ClassMethod classMethod) {
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, String variableName, int variableNameLine) {
	}

	public void visit(KeywordMessagePattern keywordMessagePattern) {
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
	}

	public void visit(Keyword keyword, String value, int line) {
	}
}
