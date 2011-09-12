/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import org.junit.*;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import javax.accessibility.AccessibleStateSet;

import java.math.BigDecimal;

import static org.easymock.EasyMock.expect;
import static org.powermock.api.easymock.PowerMock.*;

@RunWith(PowerMockRunner.class)
@PrepareForTest( { ProtoObject.class })
public class ProtoObjectTest {
	@Test
	public void integerAddition() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(1));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		mockStaticPartial(ProtoObject.class, "primitiveInteger");
		ProtoObject result = createMock(ProtoObject.class);
		// this call to primitiveInteger generates our result, which is 1 + 2
		expect(ProtoObject.primitiveInteger(integer1, new BigDecimal(1).add(new BigDecimal(2)))).andReturn(result);

		replay(integer1, integer2, result, ProtoObject.class);

		ProtoObject.primitive_21(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);
	}

	@Test
	public void integerSubtraction() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		mockStaticPartial(ProtoObject.class, "primitiveInteger");
		ProtoObject result = createMock(ProtoObject.class);
		// this call to primitiveInteger generates our result, which is 1 + 2
		expect(ProtoObject.primitiveInteger(integer1, new BigDecimal(3).subtract(new BigDecimal(2)))).andReturn(result);

		replay(integer1, integer2, result, ProtoObject.class);

		ProtoObject.primitive_22(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);
	}

	@Test
	public void integerLessThan() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(2));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(3));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_23(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfTrue, result);
	}

	@Test
	public void integerNotLessThan() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_23(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfFalse, result);
	}

	@Test
	public void integerGreaterThan() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_24(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfTrue, result);
	}

	@Test
	public void integerNotGreaterThan() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(2));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(3));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_24(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfFalse, result);
	}

	@Test
	public void integerLessThanEquals() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(3));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_25(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfTrue, result);
	}

	@Test
	public void integerNotLessThanEquals() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_25(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfFalse, result);
	}

	@Test
	public void integerGreaterThanEquals() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_26(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfTrue, result);
	}

	@Test
	public void integerNotGreaterThanEquals() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(2));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(3));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_26(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfFalse, result);
	}

	@Test
	public void integerEquals() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(3));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_27(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfTrue, result);
	}

	@Test
	public void integerNotEquals() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new BigDecimal(3));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new BigDecimal(2));

		replay(integer1, integer2);

		ProtoObject result = ProtoObject.primitive_27(integer1, null, integer2, null, null, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);

		Assert.assertEquals(ProtoObject.instanceOfFalse, result);
	}
}
