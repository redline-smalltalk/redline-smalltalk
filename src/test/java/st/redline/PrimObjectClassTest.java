/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import org.junit.Test;

import static org.junit.Assert.*;

public class PrimObjectClassTest {

	@Test
	public void shouldProvideAccessToSuperclass() {
		PrimObjectClass object = new PrimObjectClass();
		assertEquals(object.superclass(), PrimObject.PRIM_NIL);
	}

	@Test
	public void shouldProvideMutateOfSuperclass() {
		PrimObjectClass object = new PrimObjectClass();
		PrimObjectClass superclass = new PrimObjectClass();
		object.superclass(superclass);
		assertEquals(object.superclass(), superclass);
	}

	@Test
	public void shouldProvideMethodDictionary() {
		PrimObjectClass object = new PrimObjectClass();
		assertNotNull(object.methods());
	}

	@Test
	public void shouldKnowIfIncludesSelector() {
		PrimObjectClass object = new PrimObjectClass();
		object.methods().put("foo", new PrimObject());
		assertTrue(object.includesSelector("foo"));
	}

	@Test
	public void shouldFindMethodForSelector() {
		PrimObject aMethod = new PrimObject();
		PrimObjectClass object = new PrimObjectClass();
		object.methods().put("aMethod", aMethod);
		assertEquals(object.methodFor("aMethod"), aMethod);
	}

	@Test
	public void shouldKnowIfNotIncludesSelector() {
		PrimObjectClass object = new PrimObjectClass();
		assertFalse(object.includesSelector("abscentMethod"));
	}

	@Test
	public void performShouldAnswerPimitiveNilWhenNoMethodsSet() {
		PrimObjectClass object = new PrimObjectClass();
		assertEquals(object.perform("aMethod"), PrimObject.PRIM_NIL);
	}

	@Test
	public void performShouldInvokeMethodWhenMethodSet() {
		final PrimObject result = new PrimObject();
		PrimObjectClass object = new PrimObjectClass();
		object.methods().put("existingMethod", new PrimObject() {
			public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
				return result;
			}
		});
		assertEquals(object.perform(object, "existingMethod"), result);
	}
}
