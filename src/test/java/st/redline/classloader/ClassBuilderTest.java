/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import st.redline.compiler.Compiler;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ClassBuilderTest {

    private String name = "script";
    private ClassBuilder classBuilder;
    @Mock
    private SourceFinder sourceFinder;
    @Mock
    private Source source;
    @Mock
    private Compiler compiler;
    byte[] classBytes = new byte[0];

    @Before
    public void setup() {
        classBuilder = new ClassBuilder(sourceFinder, compiler);
        when(sourceFinder.find(name)).thenReturn(source);
        when(compiler.compile(source)).thenReturn(classBytes);
    }

    @Test
    public void shouldBuildClass() {
        assertEquals(classBytes, classBuilder.build(name));
    }
}
