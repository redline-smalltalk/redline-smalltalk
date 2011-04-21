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

	private static final String CLASS_NAME_SEPARATOR = "_";

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
		System.out.println("visit(Primitive)");
	}

	public void visit(Temporaries temporaries) {
		System.out.println("visit(Temporaries)");
	}

	public void visitEnd(Temporaries temporaries) {
		System.out.println("visitEnd(Temporaries)");
	}

	public void visit(Temporary temporary, int index, String value, int line) {
		System.out.println("visit(Temporary) " + value);
	}

	public void visit(VariableName variableName, String value, int line) {
		System.out.println("visit(VariableName) " + value);
		if (variableName.isClassReference())
			generator().classLookup(value, line);
		else
			throw new RuntimeException("TODO - variableName");
	}

	public void visit(Statements statements) {
		System.out.println("visit(Statements)");
	}

	public void visitEnd(Statements statements) {
		System.out.println("visitEnd(Statements)");
	}

	public void visit(AnswerExpression answerExpression) {
		System.out.println("visit(AnswerExpression)");
	}

	public void visit(Methods methods) {
		System.out.println("visit(Methods)");
	}

	public void visitEnd(Methods methods) {
		System.out.println("visitEnd(Methods)");
	}

	public void visit(InstanceMethod instanceMethod) {
		System.out.println("visit(InstanceMethod)");
		context().initializePerMethodItems();
	}

	public void visitEnd(InstanceMethod instanceMethod) {
		System.out.println("visitEnd(InstanceMethod)");
		Generator generator = generator();
		generator.closeMethod();
		generator.closeMethodClass();
		methodClasses.add(generator.classBytes());
		generator.instanceMethodBinding(sourceFileName(), methodSelector(), methodClassName());
	}

	public void visit(ClassMethod classMethod) {
		System.out.println("visit(ClassMethod)");
		context().initializePerMethodItems();
	}

	public void visitEnd(ClassMethod classMethod) {
		System.out.println("visitEnd(ClassMethod)");
		Generator generator = generator();
		generator.closeMethod();
		generator.closeMethodClass();
		methodClasses.add(generator.classBytes());
		generator.classMethodBinding(sourceFileName(), methodSelector(), methodClassName());
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
		System.out.println("visit(UnarySelectorMessagePattern) " + value);
		AnalyserContexts.AnalyserContext context = context();
		String sourceFileName = context.sourceFileName();
		String methodClassName = sourceFileName + CLASS_NAME_SEPARATOR + value;
		context.methodClassName(methodClassName);
		context.methodSelector(value);
		context.methodArgumentCount(0);
		Generator generator = generator();
		generator.openMethodClass(methodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(0);
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, String variableName, int variableNameLine) {
		System.out.println("visit(BinarySelectorMessagePattern) " + binarySelector + " " + variableName);
		AnalyserContexts.AnalyserContext context = context();
		String sourceFileName = context.sourceFileName();
		String methodClassName = sourceFileName + CLASS_NAME_SEPARATOR + binarySelector;
		context.methodClassName(methodClassName);
		context.methodSelector(binarySelector);
		context.methodArgumentCount(1);
		Generator generator = generator();
		generator.openMethodClass(methodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(1);
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
		System.out.println("visit(KeywordMessagePattern) " + keywords);
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
		System.out.println("visit(UnarySelector) " + value);
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
		System.out.println("visit(BinarySelector) " + value);
	}

	public void visit(Keyword keyword, String value, int line) {
		System.out.println("visit(Keyword) " + value);
	}

	public void visit(AssignmentExpression assignmentExpression) {
		System.out.println("visit(ArgumentExpression)");
	}

	public void visit(SimpleExpression simpleExpression) {
		System.out.println("visit(SimpleExpression)");
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		System.out.println("visitEnd(SimpleExpression)");
		if (!simpleExpression.leaveResultOnStack())
			generator().stackPop();
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		System.out.println("visit(UnarySelectorMessageElement) " + value);
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
		System.out.println("visit(BinarySelectorElement) " + value);
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions) {
		System.out.println("visit(KeywordMessageElement) " + keyword);
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
		System.out.println("visit(UnaryObjectDescription)");
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
		System.out.println("visit(BinaryObjectDescription)");
	}

	public void visit(UnaryExpression unaryExpression) {
		System.out.println("visit(UnaryExpression)");
	}

	public void visit(BinaryExpression binaryExpression) {
		System.out.println("visit(BinaryExpression)");
	}

	public void visit(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		System.out.println("visit(KeywordExpression) " + keywords);
	}

	public void visitEnd(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		System.out.println("visitEnd(KeywordExpression) " + keywords);
		generator().keywordSend(keywords, argumentCount, line);
	}

	public void visit(PrimaryExpression primaryExpression) {
		System.out.println("visit(PrimaryExpression)");
	}

	public void visit(PrimaryStatements primaryStatements) {
		System.out.println("visit(PrimaryStatements)");
	}

	public void visit(Symbol symbol, String value, int line) {
		System.out.println("visit(Symbol) " + value);
	}

	public void visit(Array array) {
		System.out.println("visit(Array)");
	}

	public void visit(Identifier identifier, String value, int line) {
		System.out.println("visit(Identifier) " + value);
	}

	public void visit(LiteralSymbol literalSymbol, String value, int line) {
		System.out.println("visit(LiteralSymbol) " + literalSymbol.value);
		generator().primitiveSymbolConversion(value, line);
	}

	public void visit(LiteralArray literalArray) {
		System.out.println("visit(LiteralArray)");
	}

	public void visit(ArrayConstantElement arrayConstantElement) {
		System.out.println("visit(ArrayConstantElement)");
	}

	public void visit(NumberConstant numberConstant) {
		System.out.println("visit(NumberConstant)");
	}

	public void visit(CharacterConstant characterConstant, String value, int line) {
		System.out.println("visit(CharacterConstant) " + value);
	}

	public void visit(StringConstant stringConstant, String value, int line) {
		System.out.println("visit(StringConstant) " + value);
	}

	public void visit(LiteralString literalString, String value, int line) {
		System.out.println("visit(LiteralString) " + value);
		generator().primitiveStringConversion(value, line);
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
		System.out.println("visit(LiteralCharacter) " + value);
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
		System.out.println("visit(NumberConstant) " + value);
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
		System.out.println("visit(LiteralNumber) " + value);
	}

	public void visit(Block block) {
		System.out.println("visit(block)");
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

	private String methodSelector() {
		return context().methodSelector();
	}

	private String methodClassName() {
		return context().methodClassName();
	}

	private String sourceFileParentPathWithoutSourcePaths() {
		return context().sourceFileParentPathWithoutSourcePaths();
	}
}
