/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader.io;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;

import java.io.File;
import java.net.URL;

import static junit.framework.Assert.assertEquals;

@RunWith(MockitoJUnitRunner.class)
public class FileSourceReaderTest {

    private FileSourceReader fileSourceReader;

    @Before
    public void setup() {
        String filename = absolutePathToThisTestClass();
        filename = filename.replace(".class", ".st");
        fileSourceReader = new FileSourceReader(filename);
    }

    private String absolutePathToThisTestClass() {
        URL thisTest = FileSourceReader.class.getResource("FileSourceReaderTest.class");
        File path = new File(thisTest.getPath());
        System.out.println("absolutePathToThisTestClass: " + path.getAbsolutePath());
        return path.getAbsolutePath();
    }

    @Test
    public void shouldReadContents() {
        assertEquals("\"I exist for the test FileSourceReaderTest.java\"\n", fileSourceReader.contents());
    }
}
