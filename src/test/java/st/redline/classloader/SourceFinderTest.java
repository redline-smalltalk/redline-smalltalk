/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import java.io.File;

import static junit.framework.Assert.assertEquals;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class SourceFinderTest {

    @Mock
    private Source source;

    @Mock
    private SourceFactory sourceFactory;
    private String[] classPaths = System.getProperty("java.class.path").split(File.pathSeparator);
    private SourceFinder sourceFinder;

    @Before
    public void setup() {
        sourceFinder = new SourceFinder(sourceFactory, classPaths);
        when(sourceFactory.createFromFile(eq("st/redline/classloader/SourceFinderFileTest.st"), notNull(String.class))).thenReturn(source);
        when(sourceFactory.createFromJar(eq("st/redline/classloader/SourceFinderJarTest.st"), notNull(String.class))).thenReturn(source);
    }

    @Test
    public void shouldFindSourceInFile() {
        assertEquals(source, sourceFinder.find("st.redline.classloader.SourceFinderFileTest"));
    }

    @Test
    public void shouldFindSourceInJar() {
        assertEquals(source, sourceFinder.find("st.redline.classloader.SourceFinderJarTest"));
    }
}
