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

	@Test public void shouldRegisterProtoObjectClass() {
		bootstrapper.bootstrap();
		verify(smalltalk).primitiveAtPut(eq("ProtoObject"), (RObject) notNull());
	}

	@Test public void shouldRegisterClassClass() {
		bootstrapper.bootstrap();
		verify(smalltalk).primitiveAtPut(eq("Class"), (RObject) notNull());
	}

	@Test public void shouldInitializeProtoObjectClassHierarchyAccordingToSmalltalkRules() {
		map = new Hashtable<String, RObject>();
		bootstrapper = new Bootstrapper(smalltalkThatCapturesBasicAtPut());
		bootstrapper.bootstrap();
		assertTrue(map.containsKey("ProtoObject"));
		RObject protoObjectClass = map.get("ProtoObject");
		assertNull(protoObjectClass.oop[RObject.SUPERCLASS_OFFSET]);
		assertNotNull(protoObjectClass.oop[RObject.CLASS_OFFSET]);
		RObject protoObjectClassMetaclass = protoObjectClass.oop[RObject.CLASS_OFFSET];
		assertNull(protoObjectClassMetaclass.oop[RObject.CLASS_OFFSET]);
		assertNotNull(protoObjectClassMetaclass.oop[RObject.SUPERCLASS_OFFSET]);
		assertEquals(protoObjectClassMetaclass.oop[RObject.SUPERCLASS_OFFSET], map.get("Class"));
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
