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

	private final List<byte[]> methodClasses;
	private final AnalyserContexts analyserContexts;

	public Analyser(Smalltalk smalltalk, Generator generator) {
		this(generator, AnalyserContexts.create(smalltalk, generator));
	}

	protected Analyser(Generator generator, AnalyserContexts analyserContexts) {
		generator.initialize();
		methodClasses = new ArrayList<byte[]>();
		this.analyserContexts = analyserContexts;
	}

	public byte[] classResult() {
		return context().classResult();
	}

	public List<byte[]> methodClassResults() {
		return methodClasses;
	}

	public void visit(Program program) {
		generator().openClass(sourceFileName(), sourceFileParentPathWithoutSourcePaths());
	}

	public void visitEnd(Program program) {
		generator().closeClass();
	}

	public void visit(Primitive primitive, String value, int line) {
	}

	public void visit(Temporaries temporaries) {
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visit(Temporary temporary, int index, String value, int line) {
	}

	public void visit(VariableName variableName, String value, int line) {
	}

	public void visit(Statements statements) {
	}

	public void visitEnd(Statements statements) {
		generator().stackPop();
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

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
	}

	public void visit(Keyword keyword, String value, int line) {
	}

	public void visit(AssignmentExpression assignmentExpression) {
	}

	public void visit(SimpleExpression simpleExpression) {
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions) {
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
	}

	public void visit(UnaryExpression unaryExpression) {
	}

	public void visit(BinaryExpression binaryExpression) {
	}

	public void visit(KeywordExpression keywordExpression) {
	}

	public void visit(PrimaryExpression primaryExpression) {
	}

	public void visit(PrimaryStatements primaryStatements) {
	}

	public void visit(Symbol symbol) {
	}

	public void visit(Array array) {
	}

	public void visit(Identifier identifier, String value, int line) {
	}

	public void visit(LiteralSymbol literalSymbol) {
	}

	public void visit(LiteralArray literalArray) {
	}

	public void visit(ArrayConstantElement arrayConstantElement) {
	}

	public void visit(NumberConstant numberConstant) {
	}

	public void visit(CharacterConstant characterConstant, String value, int line) {
	}

	public void visit(StringConstant stringConstant, String value, int line) {
	}

	public void visit(LiteralString literalString, String value, int line) {
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
	}

	public void visit(Block block) {
	}

	private AnalyserContexts.AnalyserContext context() {
		return analyserContexts.current();
	}

	private Generator generator() {
		return context().generator();
	}

	private String sourceFileName() {
		return context().sourceFileName();
	}

	private String sourceFileParentPathWithoutSourcePaths() {
		return context().sourceFileParentPathWithoutSourcePaths();
	}
}
