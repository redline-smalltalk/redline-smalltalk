/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;

public class NoOpAnalyserTest {

    NoOpAnalyser analyser;
    Analyser parent;

    @Before
    public void setup() {
        parent = mock(Analyser.class);
        analyser = new NoOpAnalyser(parent);
    }

    @Test
    public void shouldAlwaysSkipBlockVisit() {
        Block block = mock(Block.class);
        assertTrue(analyser.skipBlockVisit(block));
    }

    @Test
    public void shouldAlwaysHaveClassBytesOfZeroSize() {
        assertNotNull(analyser.classBytes());
        assertEquals(analyser.classBytes().length, 0);
    }
}
