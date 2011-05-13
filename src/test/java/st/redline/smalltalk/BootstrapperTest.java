/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline.smalltalk;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Hashtable;

import static junit.framework.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.verify;

public class BootstrapperTest {

	@Mock Smalltalk smalltalk;

	private Bootstrapper bootstrapper;
	private Hashtable<String, RObject> map;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		bootstrapper = new Bootstrapper(smalltalk);
	}

	@Test public void shouldRegisterClasses() {
		bootstrapper.bootstrap();
		verify(smalltalk).primitiveAtPut(eq("Metaclass"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("ProtoObject"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("Object"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("Behavior"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("ClassDescription"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("Class"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("Collection"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("SequenceableCollection"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("ArrayedCollection"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("String"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("Symbol"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("UndefinedObject"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("nil"), (RObject) notNull());
		verify(smalltalk).primitiveAtPut(eq("ClassBuilder"), (RObject) notNull());
	}

	@Test public void shouldInitializeProtoObjectClassHierarchyAccordingToSmalltalkRules() {
		map = new Hashtable<String, RObject>();
		bootstrapper = new Bootstrapper(smalltalkThatCapturesBasicAtPut());
		bootstrapper.bootstrap();
		assertTrue(map.containsKey("ProtoObject"));
		assertTrue(map.containsKey("Class"));
		assertTrue(map.containsKey("Metaclass"));
		assertTrue(map.containsKey("ClassDescription"));
		RObject protoObjectClass = map.get("ProtoObject");
		RObject classClass = map.get("Class");
		RObject metaclassClass = map.get("Metaclass");
		RObject classDescriptionClass = map.get("ClassDescription");
		assertEquals(null, protoObjectClass.oop[RObject.SUPERCLASS_OFFSET]);
		RObject protoObjectMetaclass = protoObjectClass.oop[RObject.CLASS_OFFSET];
		assertEquals(protoObjectMetaclass.oop[RObject.SUPERCLASS_OFFSET], classClass);
		assertEquals(protoObjectMetaclass.oop[RObject.CLASS_OFFSET], metaclassClass);
		assertEquals(classClass.oop[RObject.SUPERCLASS_OFFSET], classDescriptionClass);
		assertEquals(classClass.oop[RObject.CLASS_OFFSET].oop[RObject.SUPERCLASS_OFFSET], classDescriptionClass.oop[RObject.CLASS_OFFSET]);
	}

	@Test public void shouldInitializeBootstrapMethods() {
		map = new Hashtable<String, RObject>();
		bootstrapper = new Bootstrapper(smalltalkThatCapturesBasicAtPut());
		bootstrapper.bootstrap();
		RObject protoObjectClass = map.get("ProtoObject");
		RObject protoObjectMetaclass = protoObjectClass.oop[RObject.CLASS_OFFSET];
		RObject classClass = protoObjectMetaclass.oop[RObject.SUPERCLASS_OFFSET];
		assertNotNull(classClass.data.methodAt("subclass:instanceVariableNames:classVariableNames:classInstanceVariableNames:poolDictionaries:category:"));
	}

	@Test public void shouldInitializeClassNames() {
		map = new Hashtable<String, RObject>();
		bootstrapper = new Bootstrapper(smalltalkThatCapturesBasicAtPut());
		bootstrapper.bootstrap();
		RObject protoObjectClass = map.get("ProtoObject");
		assertEquals("ProtoObject", protoObjectClass.data.primitiveName());
	}

	@Test public void shouldInitializeClassAsBootstrapped() {
		map = new Hashtable<String, RObject>();
		bootstrapper = new Bootstrapper(smalltalkThatCapturesBasicAtPut());
		bootstrapper.bootstrap();
		RObject protoObjectClass = map.get("ProtoObject");
		assertTrue(protoObjectClass.data.isBootstrapped());
	}

	private Smalltalk smalltalkThatCapturesBasicAtPut() {
		return new SmalltalkThatCapturesBasicAtPut();
	}

	private class SmalltalkThatCapturesBasicAtPut extends Smalltalk {
		private SmalltalkThatCapturesBasicAtPut() {
			super(null, null);
		}
		protected void initialize() {
		}
		public void primitiveAtPut(String name, RObject object) {
			map.put(name, object);
		}
	}
}
