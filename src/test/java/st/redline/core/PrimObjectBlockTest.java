/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class PrimObjectBlockTest {

	private boolean blockClosureResolved = false;

	@Test
	public void shouldInitializeClassToBlockClosureWhenConstructed() {
		new TestPrimObjectBlock();
		assertTrue(blockClosureResolved);
	}

	private class TestPrimObjectBlock extends PrimObjectBlock {

		public TestPrimObjectBlock() {
			super();
		}

		PrimObject resolveBlockClosure() {
			blockClosureResolved = true;
			return null;
		}
	}
}
