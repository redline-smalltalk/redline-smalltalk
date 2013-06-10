/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import st.redline.classloader.SmalltalkClassLoader;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doReturn;

@RunWith(MockitoJUnitRunner.class)
public class ProtoObjectTest {

    @Test (expected = DoesNotUnderstandException.class)
    public void shouldPerformMessages() {
        new ProtoObject().perform("someSelector");
    }

    @Test
    public void shouldHoldJavaValue() {
        ProtoObject object =  new ProtoObject();
        String value = "a value";
        object.javaValue(value);
        assertEquals(value, object.javaValue());
    }

    @Test
    public void shouldGetClassLoader() {
        ProtoObject object =  new ProtoObject();
        ProtoObject spy = spy(object);
        SmalltalkClassLoader mock = mock(SmalltalkClassLoader.class);
        doReturn(mock).when(spy).classLoader();
        assertEquals(mock, spy.classLoader());
    }
}
