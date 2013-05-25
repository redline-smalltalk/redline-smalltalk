/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import st.redline.classloader.ClassBuilder;
import st.redline.classloader.SmalltalkClassLoader;
import st.redline.lang.ProtoClass;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class SmalltalkClassLoaderTest {

    private SmalltalkClassLoader classLoader;
    private String className = "TestClass.st";
    @Mock
    private ClassBuilder classBuilder;
    @Mock
    private Bootstrapper bootstrapper;

    @Before
    public void setup() {
        classLoader = new SmalltalkClassLoader(classBuilder, bootstrapper, false);
        when(classBuilder.build(className)).thenReturn(new byte[0]);
    }

    @Test (expected = java.lang.ClassFormatError.class)
    public void shouldLoadSmalltalkClasses() throws ClassNotFoundException {
        // We get an expected class format error because the mock ClassFinder
        // returns zero bytes.
        classLoader.loadClass(className);
    }

    @Test
    public void shouldBootstrapClassLoader() throws Exception {
        SmalltalkClassLoader classLoader = new SmalltalkClassLoader(classBuilder, bootstrapper, false);
        classLoader.bootstrap();
        verify(bootstrapper).bootstrap((SmalltalkClassLoader) any());
    }

    @Test
    public void shouldUseNonTracingClassLoaderWhenRequested() {
        SmalltalkClassLoader classLoader = new SmalltalkClassLoader(classBuilder, bootstrapper, false);
        assertEquals("st.redline.classloader.SmalltalkClassLoader$SimpleClassLoader", classLoader.delegate().getClass().getName());
    }

    @Test
    public void shouldUseTracingClassLoaderWhenRequested() {
        SmalltalkClassLoader classLoader = new SmalltalkClassLoader(classBuilder, bootstrapper, true);
        assertEquals("st.redline.classloader.SmalltalkClassLoader$TracingClassLoader", classLoader.delegate().getClass().getName());
    }
}
