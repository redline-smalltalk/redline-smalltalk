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

    public static final String OS_SPECIFIC_NAME_AND_PATH_OF_TEST_SOURCE = "st/redline/classloader/SourceFinderFileTest.st".replace("/", File.separator);
    public static final String JAR_SPECIFIC_NAME_AND_PATH_OF_TEST_SOURCE = "st/redline/classloader/SourceFinderJarTest.st";

    @Mock
    private Source source;

    @Mock
    private SourceFactory sourceFactory;
    private String[] classPaths = System.getProperty("java.class.path").split(File.pathSeparator);
    private SourceFinder sourceFinder;

    @Before
    public void setup() {
        sourceFinder = new SourceFinder(sourceFactory, classPaths);
        when(sourceFactory.createFromFile(eq(OS_SPECIFIC_NAME_AND_PATH_OF_TEST_SOURCE), notNull(String.class))).thenReturn(source);
        when(sourceFactory.createFromJar(eq(JAR_SPECIFIC_NAME_AND_PATH_OF_TEST_SOURCE), notNull(String.class))).thenReturn(source);
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
