package st.redline.core;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class PrimContextTest {

	@Test
	public void shouldHaveNoTemporariesWhenCreated() {
		PrimContext context = new PrimContext(null);
		assertNull(context.temporaries());
		context = new PrimContext(null, null, null);
		assertNull(context.temporaries());
	}

	@Test
	public void shouldInitializeTemporariesStorage() {
		PrimContext context = new PrimContext(null);
		context.temporariesInit(4);
		assertEquals(4, context.temporaries().length);
		for (int i = 0; i < context.temporaries().length; i++)
			Assert.assertEquals(PrimObject.NIL, context.temporaries()[i]);
	}
}
