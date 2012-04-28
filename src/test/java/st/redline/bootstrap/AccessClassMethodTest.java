package st.redline.bootstrap;


import org.junit.Test;
import st.redline.core.PrimObject;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class AccessClassMethodTest {
	@Test
	public void shouldAccessClassAttribute() {
		PrimObject receiver = mock(PrimObject.class);
		new AccessClassMethod().invoke(receiver, null);
		verify(receiver).cls();
	}
}
