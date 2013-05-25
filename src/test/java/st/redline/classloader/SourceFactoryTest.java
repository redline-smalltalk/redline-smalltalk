/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import st.redline.classloader.io.FileSourceReader;
import st.redline.classloader.io.JarSourceReader;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;

@RunWith(MockitoJUnitRunner.class)
public class SourceFactoryTest {

    private String sourceName = "st/redline/Thingo.st";
    private String sourceFilePath = "/home/user/classes";
    private String sourceJarPath = "/home/user/classes/things.jar";
    private SourceFactory sourceFactory;

    @Before
    public void setup() {
        sourceFactory = new SourceFactory();
    }

    @Test
    public void shouldCreateSourceFromFileWithFileReader() {
        Source source = sourceFactory.createFromFile(sourceName, sourceFilePath);
        assertNotNull(source);
        assertEquals(sourceName, source.name());
        assertEquals(sourceFilePath, source.path());
        assertTrue(source.reader() instanceof FileSourceReader);
    }

    @Test
    public void shouldCreateSourceFromJar() {
        Source source = sourceFactory.createFromJar(sourceName, sourceJarPath);
        assertNotNull(source);
        assertEquals(sourceName, source.name());
        assertEquals(sourceJarPath, source.path());
        assertTrue(source.reader() instanceof JarSourceReader);
    }
}
