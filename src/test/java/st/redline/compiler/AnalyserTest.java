/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class AnalyserTest {

	static String CLASS_NAME = "Example";
	static String PACKAGE_NAME = "com.domain";

	Analyser analyser;
	ProgramAnalyser delegate;

	@Before
	public void setup() {
		analyser = new Analyser(CLASS_NAME, PACKAGE_NAME, false);
		delegate = mock(ProgramAnalyser.class);
		analyser.currentDelegate(delegate);
	}

	@Test
	public void shouldDefaultToProgramAnalyserDelegate() {
		assertTrue(new Analyser(CLASS_NAME, PACKAGE_NAME, false).currentDelegate() instanceof ProgramAnalyser);
	}

	@Test
	public void shouldDelegateGetOfClassBytes() {
		analyser.classBytes();
		verify(delegate).classBytes();
	}

	@Test
	public void shouldDelegateVisitOfStatementsNode() {
		Statements statements = new Statements(null, null);
		statements.accept(analyser);
		verify(delegate).visitBegin(statements);
		verify(delegate).visitEnd(statements);
	}

	@Test
	public void shouldDelegateVisitOfProgramNode() {
		Program program = new Program(null, null);
		program.accept(analyser);
		verify(delegate).visitBegin(program);
		verify(delegate).visitEnd(program);
	}

	@Test
	public void shouldDelegateVisitOfTemporaryNode() {
		Temporary temporary = new Temporary("x", 32);
		temporary.accept(analyser);
		verify(delegate).visit(temporary, "x", 32);
	}
}
