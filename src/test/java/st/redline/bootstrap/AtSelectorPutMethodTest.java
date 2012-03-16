package st.redline.bootstrap;

import org.junit.Test;
import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectBlock;
import st.redline.PrimObjectClass;

import java.util.HashMap;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class AtSelectorPutMethodTest {
	@Test
	public void shouldAddBlockAtSelector() {
		HashMap methods = mock(HashMap.class);
		PrimObject value = mock(PrimObject.class);
		PrimObjectBlock block = mock(PrimObjectBlock.class);
		PrimObjectClass receiver = mock(PrimObjectClass.class);
		PrimContext context = mock(PrimContext.class);
		when(context.argumentAt(0)).thenReturn(value);
		when(context.argumentAt(1)).thenReturn(block);
		when(value.javaValue()).thenReturn("selector");
		when(receiver.methods()).thenReturn(methods);
		new AtSelectorPutMethod().invoke(receiver, context);
		verify(methods).put("selector", block);
	}
}
