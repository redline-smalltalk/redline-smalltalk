/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class NoOpAnalyserTest {

	NoOpAnalyser analyser;
	Analyser parent;

	@Before
	public void setup() {
		parent = mock(Analyser.class);
		analyser = new NoOpAnalyser(parent);
	}

//	@Test
//	public void shouldMakeRemoveNoOpAnalyserAsCurrentAnalyserWhenVisitEndOfBlock() {
//		Block block = mock(Block.class);
//		analyser.visitEnd(block, 1);
//		verify(parent).previousDelegate();
//	}
}
