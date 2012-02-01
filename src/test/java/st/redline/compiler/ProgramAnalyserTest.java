/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ProgramAnalyserTest {

	ProgramAnalyser analyser;
	ClassBytecodeWriter writer;

	@Before
	public void setup() {
		writer = mock(ClassBytecodeWriter.class);
		analyser = new ProgramAnalyser(writer);
	}

	@Test
	public void shouldInvokeArrayPutWhenVisitStringConstantInsideAnArray() {
		StringConstant stringConstant = mock(StringConstant.class);
		analyser.visit(stringConstant, "hello", 32, true, 1);
		verify(writer).invokeObjectString("hello", 1);
		verify(writer).invokeArrayPut(32, 1);
	}

	@Test
	public void shouldInvokePrimObjectStringWhenVisitStringConstant() {
		StringConstant stringConstant = mock(StringConstant.class);
		analyser.visit(stringConstant, "hello", 0, false, 1);
		verify(writer).invokeObjectString("hello", 1);
	}

	@Test
	public void shouldInvokeVariableAtWhenVisitIdentifierOnLoadSideOfExpression() {
		Identifier identifier = mock(Identifier.class);
		when(identifier.isOnLoadSideOfExpression()).thenReturn(true);
		analyser.visit(identifier, "var", 3);
		verify(writer).invokeVariableAt("var", 3);
	}

	@Test
	public void shouldInvokePerformWhenVisitEndsOfKeywordExpression() {
		KeywordExpression keywordExpression = mock(KeywordExpression.class);
		analyser.visitEnd(keywordExpression, "at:", 1, 3);
		verify(writer).invokeObjectPerform("at:", 1);
	}

	@Test
	public void shouldInvokePerformWhenVisitEndsOfKeywordMessageElementNode() {
		KeywordMessageElement keywordMessageElement = mock(KeywordMessageElement.class);
		analyser.visitEnd(keywordMessageElement, "at:put:", 2, 3);
		verify(writer).invokeObjectPerform("at:put:", 2);
	}

	@Test
	public void shouldInvokePerformWhenVisitingUnarySelectorMessageElementNode() {
		UnarySelector unarySelector = new UnarySelector("yourself", 42);
		UnarySelectorMessageElement unarySelectorMessageElement = new UnarySelectorMessageElement(unarySelector);
		analyser.visit(unarySelectorMessageElement, "yourself", 42);
		verify(writer).visitLine(42);
		verify(writer).invokeObjectPerform("yourself", 0);
	}

	@Test
	public void shouldInvokePerformWhenVisitingUnarySelectorNode() {
		UnarySelector unarySelector = new UnarySelector("yourself", 42);
		analyser.visit(unarySelector, "yourself", 42);
		verify(writer).visitLine(42);
		verify(writer).invokeObjectPerform("yourself", 0);
	}

	@Test
	public void shouldPushReceiverWhenVisitingSelfNode() {
		Self self = mock(Self.class);
		analyser.visit(self, 32);
		verify(writer).visitLine(32);
		verify(writer).pushReceiver();
	}

	@Test
	public void shouldPushNilPrimObjectFieldWhenVisitingNilNode() {
		Nil nil = mock(Nil.class);
		analyser.visit(nil, 32);
		verify(writer).visitLine(32);
		verify(writer).pushObjectStaticField("NIL");
	}

	@Test
	public void shouldPushTruePrimObjectFieldWhenVisitingTrueNode() {
		True aTrue = mock(True.class);
		analyser.visit(aTrue, 32);
		verify(writer).visitLine(32);
		verify(writer).pushObjectStaticField("TRUE");
	}

	@Test
	public void shouldPushFalsePrimObjectFieldWhenVisitingFalseNode() {
		False aFalse = mock(False.class);
		analyser.visit(aFalse, 32);
		verify(writer).visitLine(32);
		verify(writer).pushObjectStaticField("FALSE");
	}

	@Test
	public void shouldPopResultFromStackWhenSimpleExpressionResultIsNotToBeLeftOnStack() {
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		when(simpleExpression.isResultLeftOnStack()).thenReturn(false);
		when(simpleExpression.isResultDuplicatedOnStack()).thenReturn(false);
		analyser.visitEnd(simpleExpression);
		verify(writer).pop();
	}

	@Test
	public void shouldDuplicateResultOnStackWhenSimpleExpressionResultIsToBeDuplicatedOnStack() {
		SimpleExpression simpleExpression = mock(SimpleExpression.class);
		when(simpleExpression.isResultDuplicatedOnStack()).thenReturn(true);
		when(simpleExpression.isResultLeftOnStack()).thenReturn(false);
		analyser.visitEnd(simpleExpression);
		verify(writer).pushDuplicate();
	}

	@Test
	public void shouldDuplicateStackWhenCascadeBegins() {
		analyser.visitBegin(mock(Cascade.class));
		verify(writer).pushDuplicate();
	}

	@Test
	public void shouldPopStackWhenCascadeEnds() {
		analyser.visitEnd(mock(Cascade.class));
		verify(writer).pop();
	}

	@Test
	public void shouldHaveNoTemporariesRegistryWhenCreated() {
		ProgramAnalyser programAnalyser = new ProgramAnalyser("foo", "com.domain", false);
		assertNull(programAnalyser.temporariesRegistry());
		assertEquals(0, programAnalyser.temporariesIndex());
	}

	@Test
	public void shouldCreateClassBytecodeWriter() {
		assertNotNull(new ProgramAnalyser("foo", "com.domain", false).classBytecodeWriter());
	}

	@Test
	public void shouldDelegateClassBytesToClassBytecodeWriter() {
		analyser.classBytes();
		verify(writer).contents();
	}

	@Test
	public void shouldOpenClassWhenVisitingProgramNodeBegins() {
		analyser.visitBegin(mock(Program.class));
		verify(writer).openClass();
	}

	@Test
	public void shouldCloseClassWhenVisitingProgramNodeEnds() {
		analyser.visitEnd(mock(Program.class));
		verify(writer).closeClass();
	}

	@Test
	public void shouldInitializeTemporaryRegistryWhenVisitingTemporaries() {
		Temporaries temporaries = mock(Temporaries.class);
		analyser.visitBegin(temporaries);
		assertNotNull(analyser.temporariesRegistry());
		assertEquals(0, analyser.temporariesRegistry().size());
	}

	@Test
	public void shouldInitializeContextTemporariesWhenVisitingTemporaries() {
		Temporaries temporaries = mock(Temporaries.class);
		when(temporaries.size()).thenReturn(2);
		analyser.visitBegin(temporaries);
		verify(writer).invokeContextTemporariesInit(2);
	}

	@Test
	public void shouldRegisterTemporaryWhenVisitingTemporary() {
		Temporaries temporaries = mock(Temporaries.class);
		Temporary temporary = mock(Temporary.class);
		analyser.visitBegin(temporaries);
		analyser.visit(temporary, "temp", 1);
		assertNotNull(analyser.temporariesRegistry());
		assertEquals(1, analyser.temporariesRegistry().size());
		assertEquals(0, (int) analyser.temporariesRegistry().get("temp"));
		assertEquals(1, analyser.temporariesIndex());
	}
}
