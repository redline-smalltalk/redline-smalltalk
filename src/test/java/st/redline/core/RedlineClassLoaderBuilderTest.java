package st.redline.core;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import java.io.File;
import java.net.URL;
import java.util.Arrays;

import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@RunWith(MockitoJUnitRunner.class)
public class RedlineClassLoaderBuilderTest {
    @Mock
    private ClassLoader parentClassLoader;
    @Mock
    private CommandLine commandLine;
    @Mock
    private ClassLoaderCreator classloaderCreator;

    private RedlineClassLoaderBuilder redlineClassLoaderBuilder;

    @Test
    public void shouldBeAbleToCreateClassLoaderConsistsOfSmallTalkClassLoader() throws Exception {
        redlineClassLoaderBuilder.build(parentClassLoader, commandLine);

        verify(classloaderCreator).createSmalltalkClassLoader(parentClassLoader, commandLine);
    }

    @Test
    public void shouldBeAbleToCreateClassLoaderWithSourceJarFiles() throws Exception {
        File jarFile1 = File.createTempFile("src", "jar");
        URL jarUrl1 = jarFile1.toURI().toURL();
        File jarFile2 = File.createTempFile("src", "jar");
        URL jarUrl2 = jarFile2.toURI().toURL();

        when(commandLine.sourceJarFiles()).thenReturn(Arrays.asList(jarFile1, jarFile2));

        redlineClassLoaderBuilder.build(parentClassLoader, commandLine);

        verify(classloaderCreator).createUrlFileClassLoader(any(ClassLoader.class), eq(new URL[]{jarUrl1, jarUrl2}));
    }

    @Test
    public void shouldBeAbleToCreateClassLoaderThatLoadSmalltalkClassThenClassInSourceJarFilesThenClassThatIsBlock() throws Exception {
        ClassLoader smalltalkClassLoader = mock(ClassLoader.class);
        ClassLoader jarsClassLoader = mock(ClassLoader.class);
        ClassLoader exposedClassLoader = mock(ExposedClassLoader.class);

        when(classloaderCreator.createSmalltalkClassLoader(any(ClassLoader.class), any(CommandLine.class)))
                .thenReturn(smalltalkClassLoader);
        when(classloaderCreator.createUrlFileClassLoader(any(ClassLoader.class), any(URL[].class)))
                .thenReturn(jarsClassLoader);
        when(classloaderCreator.createExposedClassLoader(any(ClassLoader.class), any(CommandLine.class)))
                .thenReturn(exposedClassLoader);

        assertSame(exposedClassLoader, redlineClassLoaderBuilder.build(parentClassLoader, commandLine));

        verify(classloaderCreator).createUrlFileClassLoader(eq(smalltalkClassLoader), any(URL[].class));
        verify(classloaderCreator).createExposedClassLoader(eq(jarsClassLoader), any(CommandLine.class));
    }

    @Before
    public void setUp() throws Exception {
        redlineClassLoaderBuilder = new RedlineClassLoaderBuilder(classloaderCreator);
    }

    @After
    public void tearDown() throws Exception {
        redlineClassLoaderBuilder = null;
        parentClassLoader = null;
        commandLine = null;
        classloaderCreator = null;
    }
}
