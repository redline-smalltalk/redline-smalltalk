/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;
import st.redline.PrimObject;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.*;

public class BlockAnalyserTest {

	BlockAnalyser analyser;
	ClassBytecodeWriter writer;
	Analyser parent;
	Block block;

	@Before
	public void setup() {
		writer = mock(BlockBytecodeWriter.class);
		parent = mock(Analyser.class);
		block = mock(Block.class);
		analyser = new BlockAnalyser(parent, writer, false, block);
	}

	@Test
	public void shouldOpenClassWhenVisitBeginOfBlock() {
		analyser.visitBegin(block, 1);
		verify(writer).openClass();
	}

	@Test
	public void shouldCloseClassWhenVisitEndOfBlock() {
		analyser.visitEnd(block, 1);
		verify(writer).closeClass();
	}

	@Test
	public void shouldInvokeBlockAnswerWhenVisitAnswerStatementEnd() {
		analyser.visitEnd(mock(AnswerStatement.class));
		verify(writer).invokeBlockAnswer();
	}
}
