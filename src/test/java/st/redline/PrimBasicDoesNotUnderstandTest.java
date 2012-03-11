package st.redline;

import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.junit.Assert.*;

public class PrimBasicDoesNotUnderstandTest {

	static final String BASIC_DNU_SELECTOR = "basicDoesNotUnderstand:";

	@Test
	public void shouldSendBasicDoesNotUnderstandWhenSelectorNotBasicDoesNotUnderstand() {
		PrimObject arg1 = new PrimObject();
		PrimObject arg2 = new PrimObject();
		PrimObject response = new PrimObject();
		Fake receiver = new Fake(response, arg1, arg2, "aMethod");
		PrimBasicDoesNotUnderstand bdnu = new PrimBasicDoesNotUnderstand();
		assertEquals(bdnu.invoke(receiver, new PrimContext(receiver, null, "aMethod", arg1, arg2)), response);
	}

	@Test
	public void shouldOutputErrWhenBasicDoesNotUnderstandPassedBasicDoesNotUnderstandSelector() {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		PrintStream capturedStream = new PrintStream(os);
		System.setErr(capturedStream);
		PrimObject receiver = new PrimObject();
		PrimBasicDoesNotUnderstand bdnu = new PrimBasicDoesNotUnderstand();
		try {
			bdnu.invoke(receiver, new PrimContext(receiver, null, "aMethod"));
		} catch (RedlineException e) {
			assertEquals(e.getMessage(), "Object '" + receiver.toString() + "' (" + receiver.cls().toString() + ") does not understand 'aMethod'.");
		}
	}

	private class Fake extends PrimObject {

		private PrimObject successResponse;
		private PrimObject arg1;
		private PrimObject arg2;
		private String selector;

		public Fake(PrimObject response, PrimObject arg1, PrimObject arg2, String selector) {
			this.successResponse = response;
			this.arg1 = arg1;
			this.arg2 = arg2;
			this.selector = selector;
		}

		PrimObject perform0(PrimObject receiver, String selector, PrimObject ... arguments) {
			if (BASIC_DNU_SELECTOR.equals(selector)
					&& arguments.length == 3 && arguments[1] == arg1 && arguments[2] == arg2
					&& this.selector.equals(arguments[0].javaValue))
				return successResponse;
			return null;
		}
	}
}
