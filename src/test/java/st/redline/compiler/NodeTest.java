/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.Assert.*;
import static org.mockito.Mockito.*;

public class NodeTest {

	NodeVisitor visitor;

	@Before
	public void setup() {
		visitor = mock(NodeVisitor.class);
	}

	@Test
	public void simpleExpressionNodeWithCascadeShouldBeVisitable() {
		Cascade cascade = mock(Cascade.class);
		Primary primary = mock(Primary.class);
		MessageExpression messageExpression = mock(MessageExpression.class);
		MessageElement messageElement = mock(MessageElement.class);
		SimpleExpression.cascade = cascade;
		SimpleExpression simpleExpression = new SimpleExpression();
		simpleExpression.add(primary);
		simpleExpression.add(messageExpression);
		simpleExpression.add(messageElement);
		simpleExpression.add(messageElement);
		simpleExpression.accept(visitor);
		verify(visitor).visitBegin(simpleExpression);
		verify(primary).accept(visitor);
		verify(messageExpression).accept(visitor);
		verify(cascade, times(2)).begin(visitor);
		verify(messageElement, times(2)).accept(visitor);
		verify(cascade).end(visitor);
		verify(visitor).visitEnd(simpleExpression);
	}

	@Test
	public void statementsNodeShouldBeVisitable() {
		Expression expression = mock(Expression.class);
		Statements nestedStatements = mock(Statements.class);
		Statements statements = new Statements(expression, nestedStatements);
		statements.accept(visitor);
		verify(visitor).visitBegin(statements);
		verify(expression).accept(visitor);
		verify(nestedStatements).accept(visitor);
		verify(visitor).visitEnd(statements);
	}

	@Test
	public void programNodeShouldBeVisitable() {
		Temporaries temporaries = mock(Temporaries.class);
		Statements statements = mock(Statements.class);
		Program program = new Program(temporaries, statements);
		program.accept(visitor);
		verify(visitor).visitBegin(program);
		verify(temporaries).accept(visitor);
		verify(statements).accept(visitor);
		verify(visitor).visitEnd(program);
	}

	@Test
	public void temporariesNodeShouldBeVisitable() {
		Temporary temporary = mock(Temporary.class);
		List<Temporary> list = new ArrayList<Temporary>();
		list.add(temporary);
		Temporaries temporaries = new Temporaries(list);
		assertEquals(1, temporaries.size());
		assertFalse(temporaries.isEmpty());
		assertEquals(temporary, temporaries.get(0));
		temporaries.accept(visitor);
		verify(visitor).visitBegin(temporaries);
		verify(temporary).accept(visitor);
		verify(visitor).visitEnd(temporaries);
	}

	@Test
	public void temporaryNodeShouldBeVisitable() {
		Temporary temporary = new Temporary("x", 42);
		temporary.accept(visitor);
		verify(visitor).visit(temporary, "x", 42);
	}
}
