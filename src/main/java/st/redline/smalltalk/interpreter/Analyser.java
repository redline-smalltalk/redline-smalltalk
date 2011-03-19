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
	private static final String ADD_KEYWORD = "add:";

	private final Smalltalk smalltalk;
	private final Generator generator;
	private final List<byte[]> methodClasses;

	protected String currentMethodClassName;
	protected String currentMethodSelector;
	protected int currentMethodArgumentCount = 0;
	protected int currentMethodTemporariesCount = 0;
    protected boolean currentMethodDuplicateReceiverForCascadedMessages = false;
    protected Map<String, BasicNode> currentMethodVariableAndTemporaryRegistry = new HashMap<String, BasicNode>();  // need initial map for scripts with vars.
    protected boolean currentMethodIsClassMethod = false;
    protected int literalArrayNesting = 0;
    protected int arrayNesting = 0;
    protected int primaryExpressionNesting = 0;

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
		if (sequenceChunk.temporaries() != null)
			sequenceChunk.temporaries().accept(this);
		sequenceChunk.sequence().accept(this);
	}

	public void visit(DirectiveChunk directiveChunk) {
		directiveChunk.sequence().accept(this);
	}

	public void visit(MethodChunk methodChunk) {
		methodChunk.method().accept(this);
	}

	public void visit(Method method) {
		initializePerMethodItems();
		writeMethodClass(method);
		methodClasses.add(generator.classBytes());
		generator.methodBinding(sourceFileName(), currentMethodSelector, currentMethodClassName, currentMethodIsClassMethod);
	}

	private void initializePerMethodItems() {
		currentMethodArgumentCount = 0;
		currentMethodTemporariesCount = 0;
		currentMethodVariableAndTemporaryRegistry = new HashMap<String, BasicNode>();
	}

	private void writeMethodClass(Method method) {
		method.methodPattern().accept(this);
		if (method.temporaries() != null)
			method.temporaries().accept(this);
		if (method.pragmas() != null)
			method.pragmas().accept(this);
		method.sequence().accept(this);
		generator.closeMethod();
		generator.closeMethodClass();
	}

	public void visit(MethodPattern methodPattern) {
		methodPattern.indexArgumentsFromAndRegisterIn(2, currentMethodVariableAndTemporaryRegistry);
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
		currentMethodArgumentCount = 0;
		generator.openMethodClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(currentMethodArgumentCount);
	}

	public void visit(BinaryMethodPattern binaryMethodPattern) {
		String selector = binaryMethodPattern.selector();
		String sourceFileName = sourceFileName();
		currentMethodClassName = sourceFileName + CLASS_NAME_SEPARATOR + selector;
		currentMethodSelector = selector;
		currentMethodArgumentCount = 1;
		generator.openMethodClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(currentMethodArgumentCount);
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
		currentMethodArgumentCount = keywordArguments;
		generator.openMethodClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openMethod(currentMethodArgumentCount);
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
		if (expression.isAssignment())
			expression.assignment().accept(this);
		else if (expression.isCascade())
			expression.cascade().accept(this);
		else if (expression.isPrimary())
			expression.primary().accept(this);
		popStackIfRequired(expression);
	}

	public void visit(Assignment assignment) {
		Expression expression = assignment.expression();
		expression.accept(this);
		if (expression.isAssignment())
			generator.loadFromLocal(expression.assignment().variable().index());
		assignment.variable().accept(this);
	}

	private void popStackIfRequired(Expression expression) {
		if (!insideArrayExpression() && !insidePrimaryExpression())
			if (!expression.isAnswered() && !expression.isLast() && !expression.isRightSideOfAssignment())
				generator.stackPop();
	}

	private boolean insideArrayExpression() {
		return arrayNesting != 0;
	}

	private boolean insidePrimaryExpression() {
		return primaryExpressionNesting != 0;
	}

	public void visit(Cascade cascade) {
        currentMethodDuplicateReceiverForCascadedMessages = cascade.hasMessages();
		cascade.messageSend().accept(this);
		if (cascade.hasMessages()) {
            generator.popStackTop();
			cascade.eachAccept(this);
		}
	}

	public void visit(Message message) {
		generator.pushStackTop();
        if (message.isUnaryMessage())
            message.unaryMessage().accept(this);
        else if (message.isBinaryMessage())
            message.binaryMessage().accept(this);
        else if (message.isKeywordMessage())
            message.keywordMessage().accept(this);
        else throw new IllegalStateException("Unknown Message type.");
        generator.popStackTop();
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
		duplicateReceiverForCascadedMessages();
		unaryMessageSend.eachAccept(this);
	}

	private void duplicateReceiverForCascadedMessages() {
		if (currentMethodDuplicateReceiverForCascadedMessages) {
			currentMethodDuplicateReceiverForCascadedMessages = false;
			generator.pushStackTop();
		}
	}

	public void visit(KeywordMessageSend keywordMessageSend) {
		if (keywordMessageSend.isBinaryMessageSend() || keywordMessageSend.isUnaryMessageSend())
			throw new IllegalStateException("**** I HAVE NOT DONE THIS YET (keywordMessageSend) ****");
		keywordMessageSend.primary().accept(this);
		duplicateReceiverForCascadedMessages();
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
		if (keywordArgument.isPrimary())
			keywordArgument.primary().accept(this);
		else if (keywordArgument.isUnaryMessageSend())
			keywordArgument.unaryMessageSend().accept(this);
		else if (keywordArgument.isBinaryMessageSend())
			keywordArgument.binaryMessageSend().accept(this);
		else
			throw new IllegalStateException("Unknown KeywordArgument type.");
	}

	public void visit(BinaryMessageSend binaryMessageSend) {
		visitLeftSideOfBinaryMessageSend(binaryMessageSend);
		visitRightSideOfBinaryMessageSend(binaryMessageSend);
	}

	private void visitRightSideOfBinaryMessageSend(BinaryMessageSend binaryMessageSend) {
		binaryMessageSend.eachAccept(this);
	}

	private void visitLeftSideOfBinaryMessageSend(BinaryMessageSend binaryMessageSend) {
		if (binaryMessageSend.isPrimary()) {
			binaryMessageSend.primary().accept(this);
            duplicateReceiverForCascadedMessages();
        } else if (binaryMessageSend.isUnaryMessageSend())
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
		else {
			BasicNode reference = currentMethodVariableAndTemporaryRegistry.get(variable.name());
			if (reference == null)
				throw new IllegalStateException("Reference of undefined variable '" + variable.name() + "'.");
			if (variable.isOnLoadSideOfExpression())
				generator.loadFromLocal(reference.index());
			else
				generator.storeIntoLocal(reference.index());
		}
	}

	public void visit(PrimaryExpression primaryExpression) {
		primaryExpressionNesting++;
		primaryExpression.expression().accept(this);
		primaryExpressionNesting--;
	}

	public void visit(Symbol symbol) {
		generator.primitiveSymbolConversion(symbol.symbol(), symbol.line());
	}

	public void visit(Array array) {
		arrayNesting++;
		generator.createArray(array.line());
		array.statementList().accept(this);
		arrayNesting--;
		if (arrayNesting != 0)
			generator.keywordSend(ADD_KEYWORD, 1, array.line());
	}

	public void visit(Block block) {
		String blockName = writeBlockClass(block);
		methodClasses.add(generator.classBytes());
		generator.createBlock(blockName);
		System.out.println("**** I HAVE NOT DONE THIS YET (blockArgs) ****");
		System.out.println("**** I HAVE NOT DONE THIS YET (block temporaries) ****");
	}

	public void visit(BlockArgs blockArgs) {
		blockArgs.eachAccept(this);
	}

	public void visit(BlockArg blockArg) {
		throw new IllegalStateException("**** I HAVE NOT DONE THIS YET (blockArg) ****");
	}

	private String writeBlockClass(Block block) {
		String selector = "B1";
		String sourceFileName = sourceFileName();
		currentMethodClassName = sourceFileName + CLASS_NAME_SEPARATOR + selector;
		currentMethodSelector = selector;
		currentMethodArgumentCount = 0;
		generator.openBlockClass(currentMethodClassName, sourceFileParentPathWithoutSourcePaths(), sourceFileName);
		generator.openBlock(currentMethodArgumentCount, block.hasSequence());
		generator.closeBlock();
		generator.closeBlockClass();
		return currentMethodClassName;
	}

	public void visit(ArrayLiteral arrayLiteral) {
		generator.primitiveSymbolConversion(arrayLiteral.string(), arrayLiteral.line());
		generator.keywordSend(ADD_KEYWORD, 1, arrayLiteral.line());
	}

	public void visit(LiteralArray literalArray) {
		literalArrayNesting++;
		generator.createArray(literalArray.line());
		literalArray.eachAccept(this);
		literalArrayNesting--;
		if (literalArrayNesting != 0)
			generator.keywordSend(ADD_KEYWORD, 1, literalArray.line());
	}

	public void visit(LiteralNumber literalNumber) {
		generator.primitiveNumberConversion(literalNumber.string(), literalNumber.line());
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

	public void visit(Temporaries temporaries) {
		currentMethodTemporariesCount = temporaries.count();
		temporaries.indexFromAndRegisterIn(currentMethodArgumentCount + 2, currentMethodVariableAndTemporaryRegistry);  // + 2 because receiver is passed as argument and 'this' is at index 0.
	}

	public void visit(PragmaMessage pragmaMessage) {
		pragmaMessage.primitive().accept(this);
	}

	public void visit(PrimitiveString primitiveString) {
		generator.callToPrimitiveByString(currentMethodArgumentCount, currentMethodTemporariesCount, primitiveString.string(), primitiveString.line());
	}

	public void visit(PrimitiveNumber primitiveNumber) {
		generator.callToPrimitiveByNumber(currentMethodArgumentCount, currentMethodTemporariesCount, primitiveNumber.number(), primitiveNumber.line());
	}

	public void visit(PrimitiveModule primitiveModule) {
		generator.callToPrimitiveByModule(currentMethodArgumentCount, currentMethodTemporariesCount, primitiveModule.string(), primitiveModule.module(), primitiveModule.line());
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
