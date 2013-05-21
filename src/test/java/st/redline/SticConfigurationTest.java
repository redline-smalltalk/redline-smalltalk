/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import st.redline.classloader.Bootstrapper;
import st.redline.classloader.SmalltalkClassLoader;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class SticConfigurationTest {

    private SticConfiguration configuration;
    private String scriptName = "scriptName";
    private String[] arguments = new String[] { scriptName };
    private SticConfiguration spy;
    @Mock
    private Bootstrapper bootstrapper;

    @Before
    public void setup() {
        configuration = new SticConfiguration(arguments);
        spy = spy(configuration);
    }

    @Test
    public void shouldProvideScriptName() {
        assertEquals(scriptName, configuration.scriptName());
    }

    @Test
    public void shouldProvideSmalltalkClassLoader() throws Exception {
        assertTrue(configuration.classLoader() instanceof SmalltalkClassLoader);
    }

    @Test
    public void shouldBootstrapSmalltalkClassLoader() throws Exception {
        when(spy.bootstrapper()).thenReturn(bootstrapper);
        spy.classLoader();
        verify(bootstrapper).bootstrap((SmalltalkClassLoader) any());
    }

    @Test
    public void shouldGetClassFinderWhenConstructingClassLoader() throws Exception {
        spy.classLoader();
        verify(spy).classBuilder();
    }

    @Test
    public void shouldGetBootstrapperWhenConstructingClassLoader() throws Exception {
        spy.classLoader();
        verify(spy).bootstrapper();
    }

    @Test
    public void shouldGetClassLoaderTraceFlagWhenConstructingClassLoader() throws Exception {
        spy.classLoader();
        verify(spy).isTraceClassLoading();
    }

    @Test
    public void shouldProvideBootstrapper() {
        assertNotNull(configuration.bootstrapper());
    }

    @Test
    public void shouldGetSourceFinderWhenConstructingBootstrapper() {
        spy.bootstrapper();
        verify(spy).sourceFinder();
    }

    @Test
    public void shouldProvideClassBuilder() {
        assertNotNull(configuration.classBuilder());
    }

    @Test
    public void shouldGetSourceFinderWhenConstructingClassBuilder() {
        spy.classBuilder();
        verify(spy).sourceFinder();
    }

    @Test
    public void shouldProvideSourceFinder() {
        assertNotNull(configuration.sourceFinder());
    }

    @Test
    public void shouldGetClassPathsWhenConstructingSourceFinder() {
        spy.sourceFinder();
        verify(spy).classPaths();
    }

    @Test
    public void shouldGetSourceFactoryWhenConstructingSourceFinder() {
        spy.sourceFinder();
        verify(spy).sourceFactory();
    }

    @Test
    public void shouldProvideCompiler() {
        assertNotNull(configuration.compiler());
    }

    @Test
    public void shouldGetPreprocessorWhenConstructingCompiler() {
        spy.compiler();
        verify(spy).preprocessor();
    }

    @Test
    public void shouldGetParserWhenConstructingCompiler() {
        spy.compiler();
        verify(spy).parser();
    }

    @Test
    public void shouldGetAnalyserFactoryWhenConstructingCompiler() {
        spy.compiler();
        verify(spy).analyserFactory();
    }

    @Test
    public void shouldProvideParser() {
        assertNotNull(configuration.parser());
    }

    @Test
    public void shouldProvideAnalyserFactory() {
        assertNotNull(configuration.analyserFactory());
    }

    @Test
    public void shouldGetBytecodeFactoryWhenConstructingAnalyserFactory() {
        spy.analyserFactory();
        verify(spy).bytecodeWriterFactory();
    }

    @Test
    public void shouldGetBytecodeWritingTraceFlagWhenConstructingBytecodeFactory() {
        spy.bytecodeWriterFactory();
        verify(spy).isTraceBytecodeWriting();
    }

    @Test
    public void shouldProvideBytecodeWriterFactory() {
        assertNotNull(configuration.bytecodeWriterFactory());
    }

    @Test
    public void shouldGetTraceAnalyserFlagWhenConstructingAnalyserFactory() {
        spy.analyserFactory();
        verify(spy).isTraceAnalysis();
    }

    @Test
    public void shouldGetIgnoreParseErrorFlagWhenConstructingParser() {
        spy.parser();
        verify(spy).ignoreParseErrors();
    }

    @Test
    public void shouldProvidePreprocessor() {
        assertNotNull(configuration.preprocessor());
    }

    @Test
    public void shouldGetPreprocessorTraceFlagWhenConstructingPreprocessor() {
        spy.preprocessor();
        verify(spy).isTracePreprocessor();
    }
}
