/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

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
