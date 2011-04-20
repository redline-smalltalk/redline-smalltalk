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

import static org.mockito.Mockito.*;

public class AnalyserTest {

	@Mock Generator generator;
	@Mock AnalyserContexts.AnalyserContext analyserContext;
	@Mock AnalyserContexts analyserContexts;
	@Mock Program program;
	@Mock Statements statements;
	@Mock LiteralSymbol literalSymbol;
	@Mock LiteralString literalString;
	@Mock VariableName className;
	@Mock KeywordExpression keywordExpression;
	@Mock SimpleExpression simpleExpression;
	private Analyser analyser;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(analyserContexts.current()).thenReturn(analyserContext);
		when(analyserContext.generator()).thenReturn(generator);
		analyser = new Analyser(generator, analyserContexts);
	}

	@Test public void shouldOpenClassWhenVisitingProgram() {
		analyser.visit(program);
		verify(generator).openClass((String) any(), (String) any());
	}

	@Test public void shouldCloseClassWhenEndProgram() {
		analyser.visitEnd(program);
		verify(generator).closeClass();
	}

	@Test public void shouldPopStackWhenEndSimpleExpressionAndResultShouldNotBeLeftOnStack() {
		when(simpleExpression.leaveResultOnStack()).thenReturn(false);
		analyser.visitEnd(simpleExpression);
		verify(generator).stackPop();
	}

	@Test public void shouldLookupClassWhenVariableNameIsClassReference() {
		when(className.isClassReference()).thenReturn(true);
		analyser.visit(className, "Object", 10);
		verify(generator).classLookup("Object", 10);
	}

	@Test public void shouldConvertPrimitiveSymbols() {
		analyser.visit(literalSymbol, "Object", 10);
		verify(generator).primitiveSymbolConversion("Object", 10);
	}

	@Test public void shouldConvertPrimitiveStrings() {
		analyser.visit(literalString, "'Object'", 10);
		verify(generator).primitiveStringConversion("'Object'", 10);
	}

	@Test public void shouldInvokeKeywordSendAfterKeywordExpressionVisit() {
		analyser.visitEnd(keywordExpression, "at:", 1, 10);
		verify(generator).keywordSend("at:", 1, 10);
	}
}
