/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class PrimObjectTest {

	@Test
	public void shouldCallInvokeOnReceiverWhenP81Called() {
		PrimContext context = mock(PrimContext.class);
		PrimObject receiver = mock(PrimObject.class);
		PrimObject anObject = new PrimObject();
		anObject.p81(receiver, context);
		verify(receiver).invoke(receiver, context);
	}

	@Test
	public void shouldPassArgumentsFromP129ToAtSelectorPutMethod() {
		PrimContext context = mock(PrimContext.class);
		PrimObject aClass = new PrimObject();
		PrimObject spy = spy(aClass);
		doReturn(mock(PrimObject.class)).when(spy).atSelectorPut(spy, context);
		spy.p129(spy, context);
	}

	@Test
	public void shouldPassArgumentsFromP128ToCreateSubclassMethod() {
		PrimContext context = mock(PrimContext.class);
		PrimObject aClass = new PrimObject();
		PrimObject spy = spy(aClass);
		doReturn(mock(PrimObject.class)).when(spy).createSubclass(spy, context);
		spy.p128(spy, context);
	}

	@Test
	public void shouldCheckSizeOfInstanceWhenCreatingInstanceWithP70() {
		PrimObjectMetaclass aClass = new PrimObjectMetaclass();
		PrimObjectMetaclass spy = spy(aClass);
		when(spy.primInstanceSize()).thenReturn(4);
		PrimObject newInstance = spy.p70(spy, null);
		assertNotNull(newInstance);
		assertNotNull(newInstance.cls());
		assertEquals(5, newInstance.attributes.length);
	}

	@Test (expected = IllegalStateException.class)
	public void shouldThrowExceptionWhenBlockToBeCompiledNotFound() {
		new PrimObject().block("st/redline/Thing$M1");
	}

	@Test
	public void shouldCreateInstanceOfExistingBlockWhenBlockCalledWithNameOfRegisteredBlock() {
		PrimObject primObject = new PrimObject();
		PrimObject block = mock(PrimObject.class);
		PrimObject.BLOCKS.put("st/redline/Thing$M1", block);
		primObject.block("st/redline/Thing$M1");
		// not a test so much as documentation.
	}

	@Test
	public void shouldRaiseRedlineExceptionWhenObjectCantBeResolved() {
		try {
			new PrimObject().resolveObject("Foo");
		} catch (RedlineException e) {
			assertTrue(e.getCause() instanceof ClassNotFoundException);
			assertEquals("java.lang.ClassNotFoundException: Foo", e.getCause().toString());
		}
	}

	@Test
	public void shouldDelegatePackageLookupToClass() {
		PrimObject aClass = mock(PrimObject.class);
		PrimObject primObject = new PrimObject();
		primObject.cls(aClass);
		primObject.packageFor("SomeClass");
		verify(aClass).packageFor("SomeClass");
	}

	@Test
	public void shouldLookupImportsForFullyQualifiedClassNameWhenResolvingObjects() {
		PrimObject aClass = mock(PrimObject.class);
		PrimObject primObject = new PrimObject();
		when(aClass.packageFor("Thing")).thenReturn("st.redline.Thing");
		primObject.cls(aClass);
		try {
			primObject.resolveObject("Thing");
		} catch (RedlineException e) {
			assertEquals("st.redline.RedlineException: java.lang.ClassNotFoundException: st.redline.Thing", e.toString());
		}
	}

	@Test
	public void resolveObjectShouldReturnClassFromClassesRegistryIfFound() {
		PrimObject aClass = mock(PrimObject.class);
		PrimObject.CLASSES.put("Thing", aClass);
		PrimObject primObject = new PrimObject();
		assertEquals(aClass, primObject.resolveObject("Thing"));
	}

	@Test
	public void variableAtShouldCallResolveObject() {
		PrimObject primObject = new PrimObject();
		PrimObjectClass primObjectClass = mock(PrimObjectClass.class);
		when(primObjectClass.indexOfVariable("Thing")).thenReturn(0);
		primObject.cls(primObjectClass);
		PrimObject spy = spy(primObject);
		spy.variableAtWithin("Thing", null);
		verify(spy).resolveObject("Thing");
	}

	@Test
	public void shouldHaveClassesRegistry() {
		assertNotNull(PrimObject.CLASSES);
	}

	@Test
	public void shouldAddSlotToAttributesForClass() {
		PrimObject object = new PrimObject(5);
		assertEquals(object.attributes.length, 6);
	}

	@Test
	public void shouldInitializeAllAttributesToPrimitiveNil() {
		PrimObject object = new PrimObject(5);
		for (int i = 0; i < 5; i++)
			assertEquals(object.attributes[i], PrimObject.PRIM_NIL);
	}

	@Test
	public void shouldAddSlotForClassInAttributes() {
		PrimObject object = new PrimObject(3);
		assertEquals(object.attributes.length, 4);
	}

	@Test
	public void shouldInitializeItselfAsSuperclass() {
		PrimObject object = new PrimObject();
		assertEquals(object.superclass(), object);
	}

	@Test
	public void shouldAnswerDoesNotUnderstandAsMethodForAnySelector() {
		PrimObject object = new PrimObject();
		assertEquals(object.methodFor("foo"), PrimObject.BASIC_DOES_NOT_UNDERSTAND);
		assertEquals(object.methodFor("bar"), PrimObject.BASIC_DOES_NOT_UNDERSTAND);
	}

	@Test
	public void shouldAnswerTrueToIncludesSelector() {
		PrimObject object = new PrimObject();
		assertTrue(object.includesSelector("foo"));
		assertTrue(object.includesSelector("bar"));
	}

	@Test
	public void performShouldInvokeDnuWhenNoClassSet() {
		PrimObject object = new PrimObject();
		try {
			object.perform("anything");
		} catch (RedlineException e) {
			assertEquals(e.getMessage(), "Object '" + object.toString() + "' (" + object.cls().toString() + ") does not understand 'anything'.");
		}
	}

	@Test
	public void invokeShouldAnswerSelfWhenNoClassSet() {
		PrimObject object = new PrimObject();
		assertEquals(object, object.invoke(object, new PrimContext(object, null, "anything")));
	}

	@Test
	public void shouldCreateWithStringJavaValue() {
		PrimObjectMetaclass.BOOTSTRAPPING = true;
		String javaValue = "string";
		PrimObject object = PrimObject.string(javaValue);
		assertEquals(object.javaValue(), javaValue);
	}

	@Test
	public void shouldProvideAccessToClass() {
		PrimObject object = new PrimObject();
		assertEquals(object.cls(), PrimObject.PRIM_NIL);
	}

	@Test
	public void shouldProvideMutateOfClass() {
		PrimObject object = new PrimObject();
		PrimObject aClass = new PrimObject();
		object.cls(aClass);
		assertEquals(object.cls(), aClass);
	}
}
