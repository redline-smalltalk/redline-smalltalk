package st.redline;

import org.junit.Before;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class PrimitivesTest {

	private ProtoObject receiver;
	private ProtoObject argument;
	private ProtoObject result;
	private ProtoObject expected;
	private ProtoMethod method;

	// TODO.JCL add tests for variableAt / variableAtPut

	@Before public void setup() {
		ProtoObject.METACLASS_INSTANCE = new ProtoObject();
		ProtoObject.TRUE = new ProtoObject();
		ProtoObject.FALSE = new ProtoObject();
		ProtoObject.NIL = new ProtoObject();
		method = new ProtoMethod();
	}

	@Test public void shouldSupportPrimitiveSmallIntegerPlus() {
		receiver = protoObjectWith(new BigDecimal(1));
		argument = protoObjectWith(new BigDecimal(2));
		expected = protoObjectWith(new BigDecimal(3));
		result = Primitives.p1(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected.javaValue(), result.javaValue());
	}

	@Test public void shouldSupportPrimitiveSmallIntegerMinus() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(2));
		expected = protoObjectWith(new BigDecimal(1));
		result = Primitives.p2(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected.javaValue(), result.javaValue());
	}

	@Test public void shouldSupportPrimitiveSmallIntegerLessThan() {
		receiver = protoObjectWith(new BigDecimal(2));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p3(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerGreaterThan() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(2));
		expected = ProtoObject.TRUE;
		result = Primitives.p4(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerLessThanOrEqualTo() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p5(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerGreaterThanOrEqualTo() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p6(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerEqualTo() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p7(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerNotEqualTo() {
		// ~= not equal?
		receiver = protoObjectWith(new BigDecimal(2));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p8(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerMultiply() {
		receiver = protoObjectWith(new BigDecimal(2));
		argument = protoObjectWith(new BigDecimal(3));
		expected = protoObjectWith(new BigDecimal(6));
		result = Primitives.p9(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(((BigDecimal) expected.javaValue()).compareTo((BigDecimal) result.javaValue()), 0);
	}

	@Test public void shouldSupportPrimitiveSmallIntegerDivide() {
		receiver = protoObjectWith(new BigDecimal(6));
		argument = protoObjectWith(new BigDecimal(3));
		expected = protoObjectWith(new BigDecimal(2));
		result = Primitives.p10(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(((BigDecimal) expected.javaValue()).compareTo((BigDecimal) result.javaValue()), 0);
	}

//
//
//

	@Test public void shouldSupportPrimitiveIntegerPlus() {
		receiver = protoObjectWith(new BigDecimal(1));
		argument = protoObjectWith(new BigDecimal(2));
		expected = protoObjectWith(new BigDecimal(3));
		result = Primitives.p21(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected.javaValue(), result.javaValue());
	}

	@Test public void shouldSupportPrimitiveIntegerMinus() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(2));
		expected = protoObjectWith(new BigDecimal(1));
		result = Primitives.p22(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected.javaValue(), result.javaValue());
	}

	@Test public void shouldSupportPrimitiveIntegerLessThan() {
		receiver = protoObjectWith(new BigDecimal(2));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p23(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveIntegerGreaterThan() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(2));
		expected = ProtoObject.TRUE;
		result = Primitives.p24(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveIntegerLessThanOrEqualTo() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p25(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveIntegerGreaterThanOrEqualTo() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p26(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveIntegerEqualTo() {
		receiver = protoObjectWith(new BigDecimal(3));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p27(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveIntegerNotEqualTo() {
		// ~= not equal?
		receiver = protoObjectWith(new BigDecimal(2));
		argument = protoObjectWith(new BigDecimal(3));
		expected = ProtoObject.TRUE;
		result = Primitives.p28(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveIntegerMultiply() {
		receiver = protoObjectWith(new BigDecimal(2));
		argument = protoObjectWith(new BigDecimal(3));
		expected = protoObjectWith(new BigDecimal(6));
		result = Primitives.p29(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(((BigDecimal) expected.javaValue()).compareTo((BigDecimal) result.javaValue()), 0);
	}

	@Test public void shouldSupportPrimitiveIntegerDivide() {
		receiver = protoObjectWith(new BigDecimal(6));
		argument = protoObjectWith(new BigDecimal(3));
		expected = protoObjectWith(new BigDecimal(2));
		result = Primitives.p30(receiver, null, argument, null, null, null, null, null, null);
		assertEquals(((BigDecimal) expected.javaValue()).compareTo((BigDecimal) result.javaValue()), 0);
	}

//
//
//

	@Test public void shouldSupportPrimitiveNew() {
		receiver = new ProtoObject();
		result = Primitives.p70(receiver, null, null, null, null, null, null, null, null);
		assertNotNull(result);
		assertEquals(receiver, result.cls());
	}

//
//
//

	@Test public void shouldSupportPrimitiveSame() {
		// ==
		receiver = protoObjectWith(new BigDecimal(42));
		expected = ProtoObject.TRUE;
		result = Primitives.p110(receiver, null, receiver, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

	@Test public void shouldSupportPrimitiveClass() {
		receiver = ProtoObject.TRUE;
		receiver.cls(ProtoObject.FALSE);
		expected = receiver.cls();
		result = Primitives.p111(receiver, null, null, null, null, null, null, null, null);
		assertEquals(expected, result);
	}

//
//  Redline Primitives.
//

	@Test public void shouldProvidePackageRegistry() {
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		Primitives.packageAtPut(cls, "className", "com/example/packageName");
		assertEquals("com.example.packageName", Primitives.packageAt(cls, "className"));
	}

	@Test public void shouldAddMethods() {
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		Primitives.methodAtPut(cls, "foo", method);
		assertEquals(method, Primitives.methodAt(cls, "foo"));
	}

	@Test public void shouldFindMethodInSuperclass() {
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		ProtoObject superclass = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		cls.superclass(superclass);
		Primitives.methodAtPut(superclass, "foo", method);
		assertEquals(method, Primitives.methodAt(cls, "foo"));
	}

	@Test public void shouldSetSuperclassOfMetaclassWhenClassSuperclassSet() {
		ProtoObject fooClassClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject barClassClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject fooClass = Primitives.p70(fooClassClass, null, null, null, null, null, null, null, null);
		ProtoObject barClass = Primitives.p70(barClassClass, null, null, null, null, null, null, null, null);
		fooClass.superclass(barClass);
		assertEquals(barClassClass, fooClass.superclass().cls());
	}

	@Test public void shouldAddVariables() {
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		Primitives.addVariables(cls, "var1 var2 var3");
		assertEquals(3, cls.variables().entrySet().size());
		for (Map.Entry<String, ProtoObject> entry : cls.variables().entrySet())
			assertEquals(ProtoObject.NIL, entry.getValue());
	}

	@Test public void shouldAddInstanceVariables() {
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		Primitives.addInstanceVariables(cls, "var1 var2 var3");
		assertEquals(3, cls.instanceVariables().entrySet().size());
		for (Map.Entry<String, ProtoObject> entry : cls.instanceVariables().entrySet())
			assertEquals(ProtoObject.NIL, entry.getValue());
	}

	@Test public void shouldAddClassInstanceVariables() {
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		Primitives.addClassInstanceVariables(cls, "var1 var2 var3");
		assertEquals(3, classClass.variables().entrySet().size());
		for (Map.Entry<String, ProtoObject> entry : classClass.variables().entrySet())
			assertEquals(ProtoObject.NIL, entry.getValue());
	}

	@Test public void shouldAddEachClassInstanceVariableFromSuperclassToReceiver() {
		// the class of a class is an instance of Metaclass.
		ProtoObject superClassClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject superclass = Primitives.p70(superClassClass, null, null, null, null, null, null, null, null);
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(superclass);
		assertNotNull(cls);
		Primitives.addClassInstanceVariables(superclass, "instvar1 instvar2");
		cls.superclass(superclass);
		classClass.superclass(superClassClass);
		Primitives.addClassInstanceVariables(cls, "instvar3");
		assertEquals(3, cls.cls().variables().entrySet().size());
		for (Map.Entry<String, ProtoObject> entry : cls.cls().variables().entrySet())
			assertEquals(ProtoObject.NIL, entry.getValue());
	}

	@Test(expected = IllegalStateException.class) public void shouldNotAddDuplicateClassInstanceVariables() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		assertEquals(classClass, cls.cls());
		Primitives.addClassInstanceVariables(cls, "xxvar1 xxvar2 xxvar3");
		Primitives.addClassInstanceVariables(cls, "xxvar1");
	}

	@Test(expected = IllegalStateException.class) public void shouldNotAddDuplicateVariables() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		Primitives.addVariables(cls, "instvar1 instvar2 instvar3");
		Primitives.addVariables(cls, "instvar1");
	}

	@Test(expected = IllegalStateException.class) public void shouldNotAddVariablesThatExistInSuperclassChain() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		ProtoObject superclass = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		assertNotNull(superclass);
		cls.superclass(superclass);
		Primitives.addVariables(superclass, "instvar1 instvar2 instvar3");
		Primitives.addVariables(cls, "instvar1");
	}

	@Test(expected = IllegalStateException.class) public void shouldNotAddDuplicateInstanceVariables() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		Primitives.addInstanceVariables(cls, "instvar1 instvar2 instvar3");
		Primitives.addInstanceVariables(cls, "instvar1");
	}

	@Test(expected = IllegalStateException.class) public void shouldNotAddInstanceVariablesThatExistInSuperclassChain() {
		// the class of a class is an instance of Metaclass.
		ProtoObject classClass = Primitives.p70(ProtoObject.METACLASS_INSTANCE, null, null, null, null, null, null, null, null);
		ProtoObject cls = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		ProtoObject superclass = Primitives.p70(classClass, null, null, null, null, null, null, null, null);
		assertNotNull(cls);
		assertNotNull(superclass);
		cls.superclass(superclass);
		Primitives.addInstanceVariables(superclass, "instvar1 instvar2 instvar3");
		Primitives.addInstanceVariables(cls, "instvar1");
	}

	private ProtoObject protoObjectWith(BigDecimal bigDecimal) {
		return new ProtoObject(bigDecimal);
	}
}
