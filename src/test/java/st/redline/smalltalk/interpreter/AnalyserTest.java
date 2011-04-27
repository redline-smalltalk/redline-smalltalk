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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.*;

public class AnalyserTest {

	@Mock Generator generator;
	@Mock AnalyserContexts.AnalyserContext analyserContext;
	@Mock AnalyserContexts analyserContexts;
	@Mock Program program;
	@Mock InstanceMethod instanceMethod;
	@Mock ClassMethod classMethod;
	@Mock Statements statements;
	@Mock LiteralSymbol literalSymbol;
	@Mock LiteralString literalString;
	@Mock VariableName className;
	@Mock VariableName variableName;
	@Mock KeywordExpression keywordExpression;
	@Mock SimpleExpression simpleExpression;
	@Mock UnarySelectorMessagePattern unarySelectorMessagePattern;
	@Mock BinarySelectorMessagePattern binarySelectorMessagePattern;
	@Mock KeywordMessagePattern keywordMessagePattern;
	@Mock Temporaries temporaries;
	@Mock Primitive primitive;
	@Mock SelfReservedWord selfReservedWord;
	@Mock TrueReservedWord trueReservedWord;
	@Mock FalseReservedWord falseReservedWord;
	@Mock NilReservedWord nilReservedWord;
	private Temporary temporary;
	private List<VariableName> variableNames = new ArrayList<VariableName>();
	private Analyser analyser;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(analyserContexts.current()).thenReturn(analyserContext);
		when(analyserContext.generator()).thenReturn(generator);
		when(analyserContext.sourceFileName()).thenReturn("SourceFile");
		when(analyserContext.variableLookup("Object")).thenReturn(variableName);
		variableNames.add(variableName);
		temporary = new Temporary(new VariableName("Object", 32));
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

	@Test public void shouldInitializePerMethodItemsWhenVisitInstanceMethod() {
		analyser.visit(instanceMethod);
		verify(analyserContext).initializePerMethodItems();
	}

	@Test public void shouldInitializeMethodItemsWhenVisitUnarySelectorMessagePattern() {
		analyser.visit(unarySelectorMessagePattern, "yourself", 10);
		verify(analyserContext).methodClassName("SourceFile_yourself");
		verify(analyserContext).methodSelector("yourself");
		verify(analyserContext).methodArgumentCount(0);
	}

	@Test public void shouldOpenMethodClassWhenVisitUnarySelectorMessagePattern() {
		analyser.visit(unarySelectorMessagePattern, "yourself", 10);
		verify(generator).openMethodClass((String) any(), (String) any(), (String) any());
		verify(generator).openMethod(0);
	}

	@Test public void shouldNotRegisterMethodArgumentsWhenVisitUnarySelectorMessagePattern() {
		analyser.visit(unarySelectorMessagePattern, "yourself", 10);
		verify(analyserContext, never()).registerVariable((VariableName) any());
	}

	@Test public void shouldInitializeMethodItemsWhenVisitBinarySelectorMessagePattern() {
		analyser.visit(binarySelectorMessagePattern, "+", 10, variableName);
		verify(analyserContext).methodClassName("SourceFile_+");
		verify(analyserContext).methodSelector("+");
		verify(analyserContext).methodArgumentCount(1);
		verify(generator).openMethodClass((String) any(), (String) any(), (String) any());
		verify(generator).openMethod(1);
	}

	@Test public void shouldOpenMethodClassWhenVisitBinarySelectorMessagePattern() {
		analyser.visit(binarySelectorMessagePattern, "+", 10, variableName);
		verify(generator).openMethodClass((String) any(), (String) any(), (String) any());
		verify(generator).openMethod(1);
	}

	@Test public void shouldRegisterMethodArgumentWhenVisitBinarySelectorMessagePattern() {
		analyser.visit(binarySelectorMessagePattern, "+", 10, variableName);
		verify(analyserContext).registerVariable(variableName);
	}

	@Test public void shouldInitializeMethodItemsWhenVisitKeywordSelectorMessagePattern() {
		analyser.visit(keywordMessagePattern, "at:", 10, variableNames);
		verify(analyserContext).methodClassName("SourceFile_at:");
		verify(analyserContext).methodSelector("at:");
		verify(analyserContext).methodArgumentCount(variableNames.size());
		verify(generator).openMethodClass((String) any(), (String) any(), (String) any());
		verify(generator).openMethod(variableNames.size());
	}

	@Test public void shouldOpenMethodClassWhenVisitKeywordSelectorMessagePattern() {
		analyser.visit(keywordMessagePattern, "at:", 10, variableNames);
		verify(generator).openMethodClass((String) any(), (String) any(), (String) any());
		verify(generator).openMethod(variableNames.size());
	}

	@Test public void shouldRegisterMethodArgumentsWhenVisitKeywordSelectorMessagePattern() {
		analyser.visit(keywordMessagePattern, "at:", 10, variableNames);
		verify(analyserContext).registerVariables(variableNames);
	}

	@Test public void shouldCloseMethodClassWhenEndInstanceMethod() {
		analyser.visitEnd(instanceMethod);
		verify(generator).closeMethod();
		verify(generator).closeMethodClass();
		verify(generator).classBytes();
		verify(generator).instanceMethodBinding((String) any(), (String) any(), (String) any());
	}

	@Test public void shouldInitializePerMethodItemsWhenVisitClassMethod() {
		analyser.visit(classMethod);
		verify(analyserContext).initializePerMethodItems();
	}

	@Test public void shouldCloseMethodClassWhenEndClassMethod() {
		analyser.visitEnd(classMethod);
		verify(generator).closeMethod();
		verify(generator).closeMethodClass();
		verify(generator).classBytes();
		verify(generator).classMethodBinding((String) any(), (String) any(), (String) any());
	}

	@Test public void shouldPopStackWhenEndSimpleExpressionAndResultShouldNotBeLeftOnStack() {
		when(simpleExpression.isResultLeftOnStack()).thenReturn(false);
		analyser.visitEnd(simpleExpression);
		verify(generator).stackPop();
	}

	@Test public void shouldDuplicateStackTopWhenEndSimpleExpressionAndResultShouldBeDuplicatedOnStack() {
		when(simpleExpression.isResultDuplicatedOnStack()).thenReturn(true);
		analyser.visitEnd(simpleExpression);
		verify(generator).pushStackTop();
	}

	@Test public void shouldLookupClassWhenVariableNameIsClassReference() {
		when(className.isClassReference()).thenReturn(true);
		analyser.visit(className, "Object", 10);
		verify(generator).classLookup("Object", 10);
	}

	@Test public void shouldIndexTemporariesFromLastMethodArgumentIndex() {
		when(analyserContext.methodArgumentCount()).thenReturn(2);
		analyser.visit(temporaries);
		verify(temporaries).indexFrom(4);
		// 0 = this
		// 1 = receiver
		// 2 = arg 1
		// 3 = arg 2
		// 4 = first temporary
	}

	@Test public void shouldRegisterTemporaryVariable() {
		analyser.visit(temporary, 1, "Object", 32);
		verify(analyserContext).registerVariable(temporary);
	}

	@Test public void shouldLookupVariableNameDeclarationToGetItsProperIndex() {
		when(variableName.isClassReference()).thenReturn(false);
		analyser.visit(variableName, "Object", 10);
		verify(analyserContext).variableLookup("Object");
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

	@Test public void shouldCallPrimitiveByNumberWhenVisitingPrimitive() {
		when(analyserContext.methodArgumentCount()).thenReturn(1);
		when(analyserContext.methodTemporariesCount()).thenReturn(2);
		analyser.visit(primitive, "1", 10);
		verify(generator).callToPrimitiveByNumber(1, 2, "1", 10);
	}

	@Test public void shouldPutReceiverOnStackWhenVisitSelfReservedWord() {
		analyser.visit(selfReservedWord, 1);
		verify(generator).pushReceiver();
	}

	@Test public void shouldLookupTrueWhenVisitTrueReservedWord() {
		analyser.visit(trueReservedWord, 1);
		verify(generator).trueLookup(1);
	}

	@Test public void shouldLookupFalseWhenVisitFalseReservedWord() {
		analyser.visit(falseReservedWord, 1);
		verify(generator).falseLookup(1);
	}

	@Test public void shouldLookupNilWhenVisitNilReservedWord() {
		analyser.visit(nilReservedWord, 1);
		verify(generator).nilLookup(1);
	}
}
