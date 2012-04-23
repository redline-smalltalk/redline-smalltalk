package st.redline.bootstrap;

import org.junit.Test;
import st.redline.PrimContext;
import st.redline.PrimObject;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class CreateSubclassMethodTest {

	@Test
	public void shouldCreateSubclass() {
		// incomplete test - need to invert dependencies on CreateSubclassMethod and complete the test.
		PrimObject value = mock(PrimObject.class);
		PrimObject receiver = mock(PrimObject.class);
		PrimContext context = mock(PrimContext.class);
		when(context.argumentAt(0)).thenReturn(value);
		when(value.javaValue()).thenReturn("Class");
		new CreateSubclassMethod().invoke(receiver, context);

	}
}
