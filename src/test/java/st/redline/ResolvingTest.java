package st.redline;

import org.junit.Before;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class ResolvingTest {

	private ProtoObject classClass;
	private ProtoObject receiver;
	private ProtoObject result;
	private ProtoObject expected;

	@Before public void setup() {
		ProtoObject.METACLASS_INSTANCE = new ProtoObject();
		ProtoObject.NIL = new ProtoObject();
		classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		receiver = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
	}

	@Test(expected = RedlineException.class) public void shouldResolveObject() throws ClassNotFoundException {
		Primitives.resolveObject(receiver, "ClassName");
	}
}
