/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.*;

public class BlockAnalyserTest {

	BlockAnalyser analyser;
	ClassBytecodeWriter writer;
	Analyser parent;

	@Before
	public void setup() {
		writer = mock(ClassBytecodeWriter.class);
		parent = mock(Analyser.class);
		analyser = new BlockAnalyser(parent, writer, false);
	}

	@Test
	public void shouldMakeRemoveBlockAnalyserAsCurrentAnalyserWhenVisitEndOfBlock() {
		Block block = mock(Block.class);
		analyser.visitEnd(block, 1);
		verify(parent).previousDelegate();
	}
}
