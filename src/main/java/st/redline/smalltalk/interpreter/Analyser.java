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

	private String methodClassName;

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

		// TODO.jcl generate request to add method to dictionary of containing class.
	}

	private void writeMethodClass(Method method) {
		method.methodPattern().accept(this);
		method.sequence().accept(this);
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
		String sourceFileName = sourceFileName();
		generator.openMethodClass(sourceFileName + CLASS_NAME_SEPARATOR + unaryMethodPattern.selector(), sourceFileParentPathWithoutSourcePaths(), sourceFileName);
	}

	public void visit(BinaryMethodPattern binaryMethodPattern) {
		String sourceFileName = sourceFileName();
		generator.openMethodClass(sourceFileName + CLASS_NAME_SEPARATOR + binaryMethodPattern.selector(), sourceFileParentPathWithoutSourcePaths(), sourceFileName);
	}

	public void visit(KeywordMethodPattern keywordMethodPattern) {
		StringBuffer keywords = new StringBuffer();
		for (String keyword : keywordMethodPattern.keywords())
			keywords.append(keyword);
		String sourceFileName = sourceFileName();
		generator.openMethodClass(sourceFileName + CLASS_NAME_SEPARATOR + keywords.toString(), sourceFileParentPathWithoutSourcePaths(), sourceFileName);
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
		statementList.eachAccept(this);
	}

	public void visit(Expression expression) {
		expression.cascade().accept(this);
		if (!expression.isAnswered())
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
