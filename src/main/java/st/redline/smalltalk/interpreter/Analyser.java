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

import java.util.ArrayList;
import java.util.List;

public class Analyser implements NodeVisitor {

	private static final String CLASS_NAME_SEPARATOR = "_";

	private final Smalltalk smalltalk;
	private final Generator generator;
	private final List<byte[]> methodClasses;

	protected String currentMethodClassName;
	protected String currentMethodSelector;
	protected boolean currentMethodIsClassMethod = false;

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
		writeClass(program);
	}

	public void visit(SequenceChunk sequenceChunk) {
		sequenceChunk.sequence().accept(this);
	}

	public void visit(DirectiveChunk directiveChunk) {
		directiveChunk.sequence().accept(this);
	}

	public void visit(MethodChunk methodChunk) {
		methodChunk.method().accept(this);
	}

	public void visit(Method method) {
		writeMethodClass(method);
		methodClasses.add(generator.classBytes());
		generator.methodBinding(sourceFileName(), currentMethodSelector, currentMethodClassName, currentMethodIsClassMethod);
	}

	private void writeMethodClass(Method method) {
		method.methodPattern().accept(this);
		method.sequence().accept(this);
		generator.closeMethod();
		generator.closeMethodClass();
	}

	public void visit(MethodPattern methodPattern) {
		if (methodPattern.isUnaryMethodPattern())
			methodPattern.unaryMethodPattern().accept(this);
		else if (methodPattern.isKeywordMethodPattern())
			methodPattern.keywordMethodPattern().accept(this);
		else if (methodPattern.isBinaryMethodPattern())
			methodPattern.binaryMethodPattern().accept(this);
	}

	public void visit(UnaryMethodPattern unaryMethodPattern) {
		String selector = unaryMethodPattern.selector();
		String sourceFileName = sourceFileName();
		currentMethodClassName = sourceFileName + CLASS_NAME_SEPARATOR + selector;
		currentMethodSelector = selector;
		generator.openMethodClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(0);
	}

	public void visit(BinaryMethodPattern binaryMethodPattern) {
		String selector = binaryMethodPattern.selector();
		String sourceFileName = sourceFileName();
		currentMethodClassName = sourceFileName + CLASS_NAME_SEPARATOR + selector;
		currentMethodSelector = selector;
		generator.openMethodClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(1);
	}

	public void visit(KeywordMethodPattern keywordMethodPattern) {
		int keywordArguments = 0;
		StringBuffer keywords = new StringBuffer();
		for (String keyword : keywordMethodPattern.keywords()) {
			keywords.append(keyword);
			keywordArguments++;
		}
		String sourceFileName = sourceFileName();
		String selector = keywords.toString();
		currentMethodClassName = sourceFileName + CLASS_NAME_SEPARATOR + selector;
		currentMethodSelector = selector;
		generator.openMethodClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(keywordArguments);
	}

	private void writeClass(Program program) {
		String sourceFileName = sourceFileName();
		generator.openClass(sourceFileName, sourceFileParentPathWithoutSourcePaths());
		program.eachAccept(this);
		generator.closeClass();
	}

	public void visit(Sequence sequence) {
		sequence.statements().accept(this);
	}

	public void visit(Statements statements) {
		statements.statementList().accept(this);
	}

	public void visit(StatementList statementList) {
		statementList.markLast();
		statementList.eachAccept(this);
	}

	public void visit(Expression expression) {
		expression.cascade().accept(this);
		if (!expression.isAnswered() || !expression.isLast())
			generator.stackPop();
	}

	public void visit(Cascade cascade) {
		cascade.messageSend().accept(this);
	}

	public void visit(MessageSend messageSend) {
		if (messageSend.isUnaryMessageSend())
			messageSend.unaryMessageSend().accept(this);
		else if (messageSend.isKeywordMessageSend())
			messageSend.keywordMessageSend().accept(this);
		else if (messageSend.isBinaryMessageSend())
			messageSend.binaryMessageSend().accept(this);
	}

	public void visit(UnaryMessageSend unaryMessageSend) {
		unaryMessageSend.primary().accept(this);
		unaryMessageSend.eachAccept(this);
	}

	public void visit(KeywordMessageSend keywordMessageSend) {
		keywordMessageSend.primary().accept(this);
		keywordMessageSend.keywordMessage().accept(this);
	}

	public void visit(KeywordMessage keywordMessage) {
		visitKeywordMessageParts(keywordMessage);
		visitKeywordMessageSelector(keywordMessage);
	}

	private void visitKeywordMessageSelector(KeywordMessage keywordMessage) {
		int keywordArguments = 0;
		StringBuffer keywords = new StringBuffer();
		for (String keyword : keywordMessage.keywords()) {
			keywords.append(keyword);
			keywordArguments++;
		}
		generator.keywordSend(keywords.toString(), keywordArguments, keywordMessage.line());
	}

	private void visitKeywordMessageParts(KeywordMessage keywordMessage) {
		keywordMessage.eachAccept(this);
	}

	public void visit(KeywordMessagePart keywordMessagePart) {
		keywordMessagePart.keywordArgument().accept(this);
	}

	public void visit(KeywordArgument keywordArgument) {
		keywordArgument.primary().accept(this);
	}

	public void visit(BinaryMessageSend binaryMessageSend) {
		visitLeftSideOfBinaryMessageSend(binaryMessageSend);
		visitRightSideOfBinaryMessageSend(binaryMessageSend);
	}

	private void visitRightSideOfBinaryMessageSend(BinaryMessageSend binaryMessageSend) {
		binaryMessageSend.eachAccept(this);
	}

	private void visitLeftSideOfBinaryMessageSend(BinaryMessageSend binaryMessageSend) {
		if (binaryMessageSend.isPrimary())
			binaryMessageSend.primary().accept(this);
		else if (binaryMessageSend.isUnaryMessageSend())
			binaryMessageSend.unaryMessageSend().accept(this);
	}

	public void visit(BinaryMessage binaryMessage) {
		binaryMessage.binaryArgument().accept(this);
		generator.binarySend(binaryMessage.selector(), binaryMessage.line());
	}

	public void visit(BinaryArgument binaryArgument) {
		if (binaryArgument.isPrimary())
			binaryArgument.primary().accept(this);
		else if (binaryArgument.isUnaryMessageSend())
			binaryArgument.unaryMessageSend().accept(this);
	}

	public void visit(Variable variable) {
		if (variable.isClassReference())
			generator.classLookup(variable.name(), variable.line());
	}

	public void visit(Symbol symbol) {
		generator.primitiveSymbolConversion(symbol.symbol(), symbol.line());
	}

	public void visit(StString string) {
		generator.primitiveStringConversion(string.string(), string.line());
	}

	public void visit(StCharacter character) {
		generator.primitiveCharacterConversion(character.string(), character.line());
	}

	public void visit(Self self) {
		generator.pushReceiver();
	}

	public void visit(True literalTrue) {
		generator.trueLookup(literalTrue.line());
	}

	public void visit(False literalFalse) {
		generator.falseLookup(literalFalse.line());
	}

	public void visit(Nil nil) {
		generator.nilLookup(nil.line());
	}

	public void visit(UnaryMessage unaryMessage) {
		generator.unarySend(unaryMessage.selector(), unaryMessage.line());
	}

	private String sourceFileName() {
		return sourceFile().nameWithoutExtension();
	}

	private String sourceFileParentPathWithoutSourcePaths() {
		String parentPath = sourceFile().parentPathWithoutUserPath();
		for (String path : sourcePaths()) {
			if (parentPath.length() == path.length())
				return "";
			if (parentPath.startsWith(path))
				return parentPath.substring(path.length() + 1);
		}
		return parentPath;
	}

	private List<String> sourcePaths() {
		return smalltalk.sourcePaths();
	}

	private SourceFile sourceFile() {
		return smalltalk.currentFile();
	}
}
