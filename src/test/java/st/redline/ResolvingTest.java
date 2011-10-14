package st.redline;

import org.junit.Before;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class ResolvingTest {

	private final Primitives primitives = ProtoObject.primitives;

	private ProtoObject classClass;
	private ProtoObject receiver;
	private ProtoObject result;
	private ProtoObject expected;

	@Before public void setup() {
		ProtoObject.METACLASS_INSTANCE = new ProtoObject();
		ProtoObject.NIL = new ProtoObject();
		classClass = primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		receiver = primitives.p70(classClass, null, null, null, null, null, null, null, null);
	}

	@Test(expected = ClassNotFoundException.class) public void shouldResolveObject() throws ClassNotFoundException {
		primitives.resolveObject(receiver, "ClassName");
	}
}
