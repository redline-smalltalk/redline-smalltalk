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

import static org.mockito.Mockito.*;

public class AnalyserTest {

	private static final String PACKAGE_INTERNAL_NAME = "st/redline/smalltalk";
	private static final String CLASS_NAME = "Test";
	private static final String UNARY_MESSAGE = "new";
	private static final int LINE_NUMBER= 42;

	@Mock Smalltalk smalltalk;
	@Mock Generator generator;
	@Mock Program program;
	@Mock Sequence sequence;
	@Mock Statements statements;
	@Mock SourceFile sourceFile;
	@Mock StatementList statementList;
	@Mock Cascade cascade;
	@Mock MessageSend messageSend;
	@Mock Expression expression;
	@Mock UnaryMessageSend unaryMessageSend;
	@Mock UnaryMessage unaryMessage;
	@Mock Variable primary;
	@Mock Variable variable;
	private Analyser analyser;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		analyser = new Analyser(smalltalk, generator);
		when(program.sequence()).thenReturn(sequence);
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

	@Test public void shouldGenerateClassLookupWhenPrimaryVariableIsClassName() {
		when(variable.isClassReference()).thenReturn(true);
		when(variable.name()).thenReturn(CLASS_NAME);
		when(variable.line()).thenReturn(LINE_NUMBER);
		analyser.visit(variable);
		verify(generator).classLookup(CLASS_NAME, LINE_NUMBER);
	}

	@Test public void shouldGenerateUnarySendFromUnaryMessage() {
		when(unaryMessage.selector()).thenReturn(UNARY_MESSAGE);
		when(unaryMessage.line()).thenReturn(LINE_NUMBER);
		analyser.visit(unaryMessage);
		verify(generator).unarySend(UNARY_MESSAGE, LINE_NUMBER);
	}

	@Test public void shouldVisitChildOfProgramNode() {
		analyser.visit(program);
		verify(sequence).accept(analyser);
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

	@Test public void shouldVisitChildOfMessageSendNode() {
		analyser.visit(messageSend);
		verify(unaryMessageSend).accept(analyser);
	}

	@Test public void shouldVisitUnaryMessageSendParts() {
		when(unaryMessageSend.primary()).thenReturn(primary);
		analyser.visit(unaryMessageSend);
		verify(primary).accept(analyser);
		verify(unaryMessageSend).eachAccept(analyser);
	}
}
