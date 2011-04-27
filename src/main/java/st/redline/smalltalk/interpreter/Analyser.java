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
package st.redline.smalltalk.interpreter;

import st.redline.smalltalk.Smalltalk;

import java.util.*;

public class Analyser implements NodeVisitor {

	public static final int START_METHOD_ARGUMENT_OFFSET = 2;
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
		System.out.println("visit(Primitive) "  + value);
		AnalyserContexts.AnalyserContext context = context();
		generator().callToPrimitiveByNumber(context.methodArgumentCount(), context.methodTemporariesCount(), value, line);
	}

	public void visit(Temporaries temporaries) {
		System.out.println("visit(Temporaries)");
		context().methodTemporariesCount(temporaries.size());
		temporaries.indexFrom(START_METHOD_ARGUMENT_OFFSET + context().methodArgumentCount());
		// 0 = this.
		// 1 = receiver.
	}

	public void visitEnd(Temporaries temporaries) {
		System.out.println("visitEnd(Temporaries)");
	}

	public void visit(Temporary temporary, int index, String value, int line) {
		System.out.println("visit(Temporary) " + value + " @ " + temporary.index);
		context().registerVariable(temporary);
	}

	public void visit(VariableName variableName, String value, int line) {
		System.out.println("visit(VariableName) " + value);
		if (variableName.isClassReference())
			generator().classLookup(value, line);
		else {
			VariableName reference = context().variableLookup(value);
			if (reference == null)
				throw new IllegalStateException("Reference of undefined variable or temporary '" + value + "'.");
			if (variableName.isOnLoadSideOfExpression())
				generator().loadFromLocal(reference.index);
			else
				generator().storeIntoLocal(reference.index);
		}
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

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
		System.out.println("visit(BinarySelectorMessagePattern) " + binarySelector + " " + variableName.value);
		AnalyserContexts.AnalyserContext context = context();
		context.registerVariable(variableName);
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
		AnalyserContexts.AnalyserContext context = context();
		context.registerVariables(variableNames);
		String sourceFileName = context.sourceFileName();
		String methodClassName = sourceFileName + CLASS_NAME_SEPARATOR + keywords;
		context.methodClassName(methodClassName);
		context.methodSelector(keywords);
		context.methodArgumentCount(variableNames.size());
		Generator generator = generator();
		generator.openMethodClass(methodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(variableNames.size());
	}

	private void registerClassRelatedVariables(List<VariableName> variableNames) {
		System.out.println("** registerClassRelatedVariables ** " + variableNames);
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
		if (simpleExpression.isResultDuplicatedOnStack())
			generator().pushStackTop();
		if (!simpleExpression.isResultLeftOnStack())
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
		if (keywords.startsWith("s"))
			if (keywords.startsWith("subclass:instanceVariableNames:classVariableNames:poolDictionaries"))
				registerClassRelatedVariables(null);
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

	public void visit(SelfReservedWord selfReservedWord, int line) {
		System.out.println("visit(self)");
		generator().pushReceiver();
	}

	public void visit(TrueReservedWord trueReservedWord, int line) {
		System.out.println("visit(true)");
		generator().trueLookup(line);
	}

	public void visit(FalseReservedWord falseReservedWord, int line) {
		System.out.println("visit(false)");
		generator().falseLookup(line);
	}

	public void visit(NilReservedWord nilReservedWord, int line) {
		System.out.println("visit(nil)");
		generator().nilLookup(line);
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
