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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import st.redline.smalltalk.Smalltalk;
import st.redline.smalltalk.SourceFile;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.*;

public class AnalyserTest {

	private static final String PACKAGE_INTERNAL_NAME = "st/redline/smalltalk";
	private static final String CLASS_NAME = "Test";
	private static final String UNARY_SELECTOR = "new";
	private static final String KEYWORD_SELECTOR = "at:put:";
	private static final String CHARACTER = "$x";
	private static final String STRING = "'foo'";
	private static final String SYMBOL = "foo";
	private static final String ADD_KEYWORD = "add:";
	private static final int LINE_NUMBER= 42;
	private static final String BINARY_SELECTOR = "+";
	private static final List<String> KEYWORD_MESSAGE_LIST = new ArrayList<String>();
	static {
		KEYWORD_MESSAGE_LIST.add("at:");
		KEYWORD_MESSAGE_LIST.add("put:");
	}
	@Mock Smalltalk smalltalk;
	@Mock Generator generator;
	@Mock Program program;
	@Mock SequenceChunk sequenceChunk;
	@Mock DirectiveChunk directiveChunk;
	@Mock MethodChunk methodChunk;
	@Mock Method method;
	@Mock MethodPattern methodPattern;
	@Mock UnaryMethodPattern unaryMethodPattern;
	@Mock BinaryMethodPattern binaryMethodPattern;
	@Mock KeywordMethodPattern keywordMethodPattern;
	@Mock Sequence sequence;
	@Mock Statements statements;
	@Mock SourceFile sourceFile;
	@Mock StatementList statementList;
	@Mock Cascade cascade;
	@Mock MessageSend messageSend;
	@Mock Expression expression;
	@Mock UnaryMessageSend unaryMessageSend;
	@Mock UnaryMessage unaryMessage;
	@Mock KeywordMessageSend keywordMessageSend;
	@Mock KeywordMessage keywordMessage;
	@Mock KeywordMessagePart keywordMessagePart;
	@Mock KeywordArgument keywordArgument;
	@Mock BinaryMessageSend binaryMessageSend;
	@Mock BinaryMessage binaryMessage;
	@Mock BinaryArgument binaryArgument;
	@Mock StString string;
	@Mock Symbol symbol;
	@Mock StCharacter character;
	@Mock Variable primary;
	@Mock Self self;
	@Mock True literalTrue;
	@Mock False literalFalse;
	@Mock Nil nil;
	@Mock Variable variable;
	@Mock LiteralArray literalArray;
	@Mock ArrayLiteral arrayLiteral;
	private Analyser analyser;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		analyser = new Analyser(smalltalk, generator);
		when(sequenceChunk.sequence()).thenReturn(sequence);
		when(directiveChunk.sequence()).thenReturn(sequence);
		when(methodChunk.method()).thenReturn(method);
		when(method.methodPattern()).thenReturn(methodPattern);
		when(method.sequence()).thenReturn(sequence);
		when(sequence.statements()).thenReturn(statements);
		when(statements.statementList()).thenReturn(statementList);
		when(expression.cascade()).thenReturn(cascade);
		when(cascade.messageSend()).thenReturn(messageSend);
		when(messageSend.unaryMessageSend()).thenReturn(unaryMessageSend);
		when(smalltalk.currentFile()).thenReturn(sourceFile);
		when(sourceFile.nameWithoutExtension()).thenReturn(CLASS_NAME);
		when(sourceFile.parentPathWithoutUserPath()).thenReturn(PACKAGE_INTERNAL_NAME);
	}

	@Test public void shouldGenerateProgramFromProgramNode() {
		analyser.visit(program);
		verify(generator).openClass(CLASS_NAME, PACKAGE_INTERNAL_NAME);
		verify(generator).closeClass();
	}

	@Test public void shouldCloseMethodClassAfterMethodVisit() {
		analyser.visit(method);
		verify(methodPattern).accept(analyser);
		verify(sequence).accept(analyser);
		verify(generator).closeMethod();
		verify(generator).closeMethodClass();
		verify(generator).classBytes();
	}

	@Test public void shouldGenerateMethodBindAfterMethodVisit() {
		analyser.currentMethodSelector = UNARY_SELECTOR;
		analyser.currentMethodClassName = CLASS_NAME + "_" + UNARY_SELECTOR;
		analyser.currentMethodIsClassMethod = false;
		analyser.visit(method);
		verify(generator).methodBinding(CLASS_NAME, UNARY_SELECTOR, CLASS_NAME + "_" + UNARY_SELECTOR, false);
	}

	@Test public void shouldGenerateMethodClassNameFromUnaryMethodPattern() {
		when(unaryMethodPattern.selector()).thenReturn(UNARY_SELECTOR);
		analyser.visit(unaryMethodPattern);
		verify(generator).openMethodClass(CLASS_NAME + "_" + UNARY_SELECTOR, PACKAGE_INTERNAL_NAME, CLASS_NAME);
		verify(generator).openMethod(0);
	}

	@Test public void shouldGenerateMethodClassNameFromBinaryMethodPattern() {
		when(binaryMethodPattern.selector()).thenReturn(BINARY_SELECTOR);
		analyser.visit(binaryMethodPattern);
		verify(generator).openMethodClass(CLASS_NAME + "_" + BINARY_SELECTOR, PACKAGE_INTERNAL_NAME, CLASS_NAME);
		verify(generator).openMethod(1);
	}

	@Test public void shouldGenerateMethodClassNameFromKeywordMethodPattern() {
		when(keywordMethodPattern.keywords()).thenReturn(KEYWORD_MESSAGE_LIST);
		analyser.visit(keywordMethodPattern);
		verify(generator).openMethodClass(CLASS_NAME + "_" + KEYWORD_SELECTOR, PACKAGE_INTERNAL_NAME, CLASS_NAME);
		verify(generator).openMethod(2);
	}

	@Test public void shouldGenerateClassLookupWhenPrimaryVariableIsClassName() {
		when(variable.isClassReference()).thenReturn(true);
		when(variable.name()).thenReturn(CLASS_NAME);
		when(variable.line()).thenReturn(LINE_NUMBER);
		analyser.visit(variable);
		verify(generator).classLookup(CLASS_NAME, LINE_NUMBER);
	}

	@Test public void shouldGenerateUnarySendFromUnaryMessage() {
		when(unaryMessage.selector()).thenReturn(UNARY_SELECTOR);
		when(unaryMessage.line()).thenReturn(LINE_NUMBER);
		analyser.visit(unaryMessage);
		verify(generator).unarySend(UNARY_SELECTOR, LINE_NUMBER);
	}

	@Test public void shouldGenerateKeywordSendFromKeywordMessage() {
		when(keywordMessage.keywords()).thenReturn(KEYWORD_MESSAGE_LIST);
		when(keywordMessage.line()).thenReturn(LINE_NUMBER);
		analyser.visit(keywordMessage);
		verify(keywordMessage).eachAccept(analyser);
		verify(generator).keywordSend(KEYWORD_SELECTOR, 2, LINE_NUMBER);
	}

	@Test public void shouldGeneratePopWhenExpressionResultNotAnswered() {
		when(expression.isAnswered()).thenReturn(false);
		analyser.visit(expression);
		verify(generator).stackPop();
	}

	@Test public void shouldGeneratePopWhenNotLastExpression() {
		when(expression.isAnswered()).thenReturn(false);
		when(expression.isLast()).thenReturn(false);
		analyser.visit(expression);
		verify(generator).stackPop();
	}

	@Test public void shouldGeneratePrimitiveConversionForCharacter() {
		when(character.string()).thenReturn(CHARACTER);
		when(character.line()).thenReturn(LINE_NUMBER);
		analyser.visit(character);
		verify(generator).primitiveCharacterConversion(CHARACTER, LINE_NUMBER);
	}

	@Test public void shouldGeneratePrimitiveConversionForString() {
		when(string.string()).thenReturn(STRING);
		when(string.line()).thenReturn(LINE_NUMBER);
		analyser.visit(string);
		verify(generator).primitiveStringConversion(STRING, LINE_NUMBER);
	}

	@Test public void shouldGeneratePrimitiveConversionForSymbol() {
		when(symbol.symbol()).thenReturn(SYMBOL);
		when(symbol.line()).thenReturn(LINE_NUMBER);
		analyser.visit(symbol);
		verify(generator).primitiveSymbolConversion(SYMBOL, LINE_NUMBER);
	}

	@Test public void shouldGeneratePushOfThisForSelf() {
		analyser.visit(self);
		verify(generator).pushReceiver();
	}

	@Test public void shouldGenerateTrueLookupForLiteralTrue() {
		when(literalTrue.line()).thenReturn(LINE_NUMBER);
		analyser.visit(literalTrue);
		verify(generator).trueLookup(LINE_NUMBER);
	}

	@Test public void shouldGenerateFalseLookupForLiteralFalse() {
		when(literalFalse.line()).thenReturn(LINE_NUMBER);
		analyser.visit(literalFalse);
		verify(generator).falseLookup(LINE_NUMBER);
	}

	@Test public void shouldGenerateNilLookupForNil() {
		when(nil.line()).thenReturn(LINE_NUMBER);
		analyser.visit(nil);
		verify(generator).nilLookup(LINE_NUMBER);
	}

	@Test public void shouldGenerateArrayLookupForLiteralArray() {
		when(literalArray.line()).thenReturn(LINE_NUMBER);
		analyser.visit(literalArray);
		verify(generator).createArray(LINE_NUMBER);
		verify(literalArray).eachAccept(analyser);
	}

	@Test public void shouldNotGenerateAddAfterLiteralArrayWhenLiteralArrayNotNested() {
		when(literalArray.line()).thenReturn(LINE_NUMBER);
		analyser.visit(literalArray);
		verify(generator).createArray(LINE_NUMBER);
		verify(literalArray).eachAccept(analyser);
		verify(generator, never()).keywordSend(ADD_KEYWORD, 1, LINE_NUMBER);
	}

	@Test public void shouldGenerateAddAfterLiteralArrayWhenNestedLiteralArray() {
		when(literalArray.line()).thenReturn(LINE_NUMBER);
		analyser.literalArrayNesting = 2;
		analyser.visit(literalArray);
		verify(generator).createArray(LINE_NUMBER);
		verify(generator).keywordSend(ADD_KEYWORD, 1, LINE_NUMBER);
		verify(literalArray).eachAccept(analyser);
	}

	@Test public void shouldGenerateCreationAndStoreForArrayLiteral() {
		when(arrayLiteral.line()).thenReturn(LINE_NUMBER);
		when(arrayLiteral.string()).thenReturn(SYMBOL);
		analyser.visit(arrayLiteral);
		verify(generator).primitiveSymbolConversion(SYMBOL, LINE_NUMBER);
		verify(generator).keywordSend(ADD_KEYWORD, 1, LINE_NUMBER);
	}

	@Test public void shouldVisitEachNodeOfProgramNode() {
		analyser.visit(program);
		verify(program).eachAccept(analyser);
	}

	@Test public void shouldVisitChildOfSequenceChunkNode() {
		analyser.visit(sequenceChunk);
		verify(sequence).accept(analyser);
	}

	@Test public void shouldVisitChildOfDirectiveChunkNode() {
		analyser.visit(directiveChunk);
		verify(sequence).accept(analyser);
	}

	@Test public void shouldVisitChildOfMethodChunkNode() {
		analyser.visit(methodChunk);
		verify(method).accept(analyser);
	}

	@Test public void shouldVisitChildrenOfMethodNode() {
		analyser.visit(method);
		verify(methodPattern).accept(analyser);
		verify(sequence).accept(analyser);
	}

	@Test public void shouldVisitUnaryMethodPatternWhenMethodPatternIsUnaryType() {
		when(methodPattern.isUnaryMethodPattern()).thenReturn(true);
		when(methodPattern.unaryMethodPattern()).thenReturn(unaryMethodPattern);
		analyser.visit(methodPattern);
		verify(unaryMethodPattern).accept(analyser);
	}

	@Test public void shouldVisitBinaryMethodPatternWhenMethodPatternIsBinaryType() {
		when(methodPattern.isBinaryMethodPattern()).thenReturn(true);
		when(methodPattern.binaryMethodPattern()).thenReturn(binaryMethodPattern);
		analyser.visit(methodPattern);
		verify(binaryMethodPattern).accept(analyser);
	}

	@Test public void shouldVisitKeywordMethodPatternWhenMethodPatternIsKeywordType() {
		when(methodPattern.isKeywordMethodPattern()).thenReturn(true);
		when(methodPattern.keywordMethodPattern()).thenReturn(keywordMethodPattern);
		analyser.visit(methodPattern);
		verify(keywordMethodPattern).accept(analyser);
	}

	@Test public void shouldVisitChildOfSequenceNode() {
		analyser.visit(sequence);
		verify(statements).accept(analyser);
	}

	@Test public void shouldVisitChildOfStatementsNode() {
		analyser.visit(statements);
		verify(statementList).accept(analyser);
	}

	@Test public void shouldVisitEachNodeOfStatementListNode() {
		analyser.visit(statementList);
		verify(statementList).markLast();
		verify(statementList).eachAccept(analyser);
	}

	@Test public void shouldVisitExpressionNode() {
		analyser.visit(expression);
		verify(cascade).accept(analyser);
	}

	@Test public void shouldVisitChildOfCascadeNode() {
		analyser.visit(cascade);
		verify(messageSend).accept(analyser);
	}

	@Test public void shouldVisitUnaryMessageSendWhenMessageSendIsUnaryType() {
		when(messageSend.isUnaryMessageSend()).thenReturn(true);
		when(messageSend.unaryMessageSend()).thenReturn(unaryMessageSend);
		analyser.visit(messageSend);
		verify(unaryMessageSend).accept(analyser);
	}

	@Test public void shouldVisitKeywordMessageSendWhenMessageSendIsKeywordType() {
		when(messageSend.isKeywordMessageSend()).thenReturn(true);
		when(messageSend.keywordMessageSend()).thenReturn(keywordMessageSend);
		analyser.visit(messageSend);
		verify(keywordMessageSend).accept(analyser);
	}

	@Test public void shouldVisitKeywordMessageSendParts() {
		when(keywordMessageSend.primary()).thenReturn(primary);
		when(keywordMessageSend.keywordMessage()).thenReturn(keywordMessage);
		analyser.visit(keywordMessageSend);
		verify(primary).accept(analyser);
		verify(keywordMessage).accept(analyser);
	}

	@Test public void shouldVisitBinaryMessageSendWhenMessageSendIsBinaryType() {
		when(messageSend.isBinaryMessageSend()).thenReturn(true);
		when(messageSend.binaryMessageSend()).thenReturn(binaryMessageSend);
		analyser.visit(messageSend);
		verify(binaryMessageSend).accept(analyser);
	}

	@Test public void shouldVisitPrimaryWhenLeftOfBinaryMessageSendIsPrimary() {
		when(binaryMessageSend.isPrimary()).thenReturn(true);
		when(binaryMessageSend.primary()).thenReturn(primary);
		analyser.visit(binaryMessageSend);
		verify(primary).accept(analyser);
		verify(binaryMessageSend).eachAccept(analyser);
	}

	@Test public void shouldVisitUnaryMessageSendWhenLeftOfBinaryMessageSendIsUnaryMessageSend() {
		when(binaryMessageSend.isUnaryMessageSend()).thenReturn(true);
		when(binaryMessageSend.unaryMessageSend()).thenReturn(unaryMessageSend);
		analyser.visit(binaryMessageSend);
		verify(unaryMessageSend).accept(analyser);
		verify(binaryMessageSend).eachAccept(analyser);
	}

	@Test public void shouldVisitEachBinaryMessageOfBinaryMessageSend() {
		when(binaryMessageSend.isUnaryMessageSend()).thenReturn(true);
		when(binaryMessageSend.unaryMessageSend()).thenReturn(unaryMessageSend);
		analyser.visit(binaryMessageSend);
		verify(binaryMessageSend).eachAccept(analyser);
	}

	@Test public void shouldVisitBinaryMessageArgumentBeforeGeneratingSend() {
		when(binaryMessage.binaryArgument()).thenReturn(binaryArgument);
		when(binaryMessage.selector()).thenReturn(BINARY_SELECTOR);
		when(binaryMessage.line()).thenReturn(LINE_NUMBER);
		analyser.visit(binaryMessage);
		verify(binaryArgument).accept(analyser);
		verify(generator).binarySend(BINARY_SELECTOR, LINE_NUMBER);
	}

	@Test public void shouldVisitPrimaryWhenBinaryArgumentIsPrimary() {
		when(binaryArgument.isPrimary()).thenReturn(true);
		when(binaryArgument.primary()).thenReturn(primary);
		analyser.visit(binaryArgument);
		verify(primary).accept(analyser);
	}

	@Test public void shouldVisitUnaryMessageSendWhenBinaryArgumentIsUnaryMessageSend() {
		when(binaryArgument.isPrimary()).thenReturn(false);
		when(binaryArgument.isUnaryMessageSend()).thenReturn(true);
		when(binaryArgument.unaryMessageSend()).thenReturn(unaryMessageSend);
		analyser.visit(binaryArgument);
		verify(unaryMessageSend).accept(analyser);
	}

	@Test public void shouldVisitEachNodeOfKeywordMessageListNode() {
		analyser.visit(keywordMessage);
		verify(keywordMessage).eachAccept(analyser);
	}

	@Test public void shouldVisitKeywordMessagePart() {
		when(keywordMessagePart.keywordArgument()).thenReturn(keywordArgument);
		analyser.visit(keywordMessagePart);
		verify(keywordArgument).accept(analyser);
	}

	@Test public void shouldVisitUnaryMessageSendParts() {
		when(unaryMessageSend.primary()).thenReturn(primary);
		analyser.visit(unaryMessageSend);
		verify(primary).accept(analyser);
		verify(unaryMessageSend).eachAccept(analyser);
	}
}
