/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader.io;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;

import java.io.File;

import static org.junit.Assert.assertEquals;

@RunWith(MockitoJUnitRunner.class)
public class JarSourceReaderTest {

    private JarSourceReader jarSourceReader;

    @Before
    public void setup() {
        String jarPath = absolutePathToTestJar();
        String entryName = "st.redline.classloader.SourceFinderJarTest".replace(".", "/");
        entryName = entryName + ".st";
        jarSourceReader = new JarSourceReader(entryName, jarPath);
    }

    private String absolutePathToTestJar() {
        for (String path : System.getProperty("java.class.path").split(File.pathSeparator))
            if (path.endsWith("SourceFinderTest.jar"))
                return path;
        return null;
    }

    @Test
    public void shouldReadContents() {
        assertEquals("\"I exist for the SourceFileFinderTest.java class to find.\"", jarSourceReader.contents().trim());
    }
}
