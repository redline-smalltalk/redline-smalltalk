/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class SticTest {

    @Mock
    private ClassLoader classLoader;
    @Mock
    private SticConfiguration configuration;
    String scriptName = "script";
    private Stic stic;

    @Before
    public void setup() throws Exception {
        stic = new Stic(configuration);
        when(configuration.classLoader()).thenReturn(classLoader);
        when(configuration.scriptName()).thenReturn(scriptName);
    }

    @Test
    public void shouldExecuteScriptWithSmalltalkClassLoader() throws Exception {
        try {
            stic.run();
        } catch (ClassNotFoundException e) {
        }
        verify(configuration).scriptName();
        verify(configuration).classLoader();
    }
}
