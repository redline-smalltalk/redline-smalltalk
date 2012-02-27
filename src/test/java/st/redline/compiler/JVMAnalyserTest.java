/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import static junit.framework.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class JVMAnalyserTest {

	JVMAnalyser analyser;
	ClassBytecodeWriter writer;
	Analyser parent;

	@Before
	public void setup() {
		writer = mock(ClassBytecodeWriter.class);
		parent = mock(Analyser.class);
		analyser = new JVMAnalyser(parent, writer, false);
	}

	@Test
	public void shouldVisitVarInsnWhenKeywordExpressionAload() {
		analyser.visitBegin(mock(KeywordExpression.class), "aload:", 1, 0);
		assertNotNull(analyser.builder);
		assertTrue(analyser.builder instanceof JVMAnalyser.VisitVarInsnBuilder);
	}

	@Test
	public void shouldAddStringConstantAsBuilderArgument() {
		analyser.builder = mock(JVMAnalyser.Builder.class);
		analyser.visit(mock(StringConstant.class), "literal string", 0, false, 0);
		verify(analyser.builder).addArgument("literal string");
	}

	@Test
	public void shouldVisitMethodInsnWhenKeywordExpressionInvokeVirtual() {
		analyser.visitBegin(mock(KeywordExpression.class), "invokeVirtual:method:returning:", 3, 0);
		assertNotNull(analyser.builder);
		assertTrue(analyser.builder instanceof JVMAnalyser.VisitMethodInsnBuilder);
	}

	@Test
	public void shouldVisitLdcInsnWhenKeywordExpressionLds() {
		analyser.visitBegin(mock(KeywordExpression.class), "ldc:", 1, 0);
		assertNotNull(analyser.builder);
		assertTrue(analyser.builder instanceof JVMAnalyser.VisitLdcInsnBuilder);
	}

    @Test
    public void shouldVisitFieldInsnWithOpcodeGetStaticWhenKeywordExpressionMatches() {
        analyser.visitBegin(mock(KeywordExpression.class), "getStatic:named:as:", 3, 0);
        assertNotNull(analyser.builder);
        assertTrue(analyser.builder instanceof JVMAnalyser.VisitFieldInsnBuilder);
    }

    @Test
    public void shouldWriteCurrentInstructionWhenVisitOfKeywordExpressionEnds() {
        analyser.builder = mock(JVMAnalyser.Builder.class);
        analyser.visitEnd(mock(KeywordExpression.class), "getStatic:named:as:", 3, 0);
        verify(analyser.builder).writeUsing(writer);
    }

    @Test (expected = IllegalStateException.class)
    public void shouldBeAnExceptionToVisitUnsupportedKeywordExpression() {
        analyser.visitBegin(mock(KeywordExpression.class), "unsupportedKeyword:", 1, 0);
    }

    @Test
    public void shouldVisitFieldInsnWithOpcodeGetStaticWhenKeywordMessageElementMatches() {
        analyser.visitBegin(mock(KeywordMessageElement.class), "getStatic:named:as:", 3, 0);
        assertNotNull(analyser.builder);
        assertTrue(analyser.builder instanceof JVMAnalyser.VisitFieldInsnBuilder);
    }

    @Test
    public void shouldWriteCurrentInstructionWhenVisitOfKeywordMessageElementEnds() {
        analyser.builder = mock(JVMAnalyser.Builder.class);
        analyser.visitEnd(mock(KeywordMessageElement.class), "getStatic:named:as:", 3, 0);
        verify(analyser.builder).writeUsing(writer);
    }

    @Test (expected = IllegalStateException.class)
    public void shouldBeAnExceptionToVisitUnsupportedKeywordMessageElement() {
        analyser.visitBegin(mock(KeywordMessageElement.class), "unsupportedKeyword:", 1, 0);
    }

	@Test
	public void shouldRestorePreviousAnalyserWhenVisitLastStatementEnd() {
		analyser.visitEnd(mock(Statements.class));
		verify(parent).previousDelegate();
	}

	@Test
	public void shouldWriteVisitInstructionForUnarySelectors() {
		analyser.visit(mock(UnarySelector.class), "areturn", 32);
		verify(writer).visitLine(32);
		verify(writer).visitInsn("ARETURN");
	}

	@Test
	public void shouldWriteVisitLineForVisitOfJVM() {
		analyser.visit(mock(JVM.class), 32);
		verify(writer).visitLine(32);
	}
}
