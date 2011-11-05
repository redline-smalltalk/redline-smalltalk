package st.redline;

import org.junit.Before;
import org.junit.Test;

import java.math.BigInteger;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class ConstructionTest {

	private ProtoObject receiver;
	private ProtoObject result;
	private ProtoObject expected;

	@Before public void setup() {
		ProtoObject.METACLASS_INSTANCE = new ProtoObject();
		ProtoObject.NIL = new ProtoObject();
	}

	@Test public void shouldCreateNewMetaclass() {
		receiver = ProtoObject.METACLASS_INSTANCE;
		result = Primitives.p70(receiver, null, null, null, null, null, null, null, null);
		expected = receiver;
		assertNotNull(result);
		assertEquals(expected, result.cls());
	}

	@Test public void shouldCreateInstanceOfClass() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		assertEquals(classClass, cls.cls());
		assertEquals(ProtoObject.METACLASS_INSTANCE, cls.cls().cls());
	}

	@Test public void shouldCreateInstanceOfClassWithInstanceVariables() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		Primitives.addInstanceVariables(cls, "instvar1 instvar2 instvar3");
		ProtoObject instance = Primitives.p70(cls, null, null, null, null, null, null, null, null);
		assertNotNull(instance.variables());
		assertEquals(3, instance.variables().entrySet().size());
		for (Map.Entry<String, ProtoObject> entry : instance.variables().entrySet())
			assertEquals(ProtoObject.NIL, entry.getValue());
	}

	@Test public void shouldCreateInstanceOfClassWithInstanceVariablesFromSuperclass() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject superclass = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertEquals(classClass, superclass.cls());
		Primitives.addInstanceVariables(superclass, "instvar1 instvar2 instvar3");
		assertNotNull(superclass.instanceVariables());
		assertEquals(3, superclass.instanceVariables().size());
		ProtoObject otherClassClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(otherClassClass, null, null, null, null, null, null, null, null);
		assertEquals(otherClassClass, cls.cls());
		cls.superclass(superclass);
		assertEquals(superclass, cls.superclass());
		assertEquals(cls.superclass().cls(), superclass.cls());
		ProtoObject instance = Primitives.p70(cls, null, null, null, null, null, null, null, null);
		assertNotNull(instance.variables());
		assertEquals(3, instance.variables().entrySet().size());
		for (Map.Entry<String, ProtoObject> entry : instance.variables().entrySet())
			assertEquals(ProtoObject.NIL, entry.getValue());
	}
}
