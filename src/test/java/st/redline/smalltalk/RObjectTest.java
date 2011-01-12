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

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class RObjectTest {

	private static final String MESSAGE = "example";

	@Mock RMethod method;
	@Mock RData classData;
	@Mock RData superclassData;

	private RObject object;
	private RObject classObject;
	private RObject superclassObject;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		classObject = new RObject(true);
		superclassObject = new RObject(true);
	}

	@Test public void shouldCreateBasicClass() {
		object = new RObject(true);
		assertEquals(2, object.oop.length);
		assertNotNull(object.data);
		assertNull(object.data.methodAt("nonExistentSelector"));
	}

	@Test public void shouldCreateBasicInstance() {
		object = new RObject(false);
		assertEquals(1, object.oop.length);
		assertNotNull(object.data);
	}

	@Test (expected=IllegalStateException.class)
	public void shouldNotHaveMethodDictionaryInInstance() {
		object = new RObject(false);
		object.data.methodAt("anySelector");
	}

	@Test public void shouldLookupMethodInClassOfReceiver() {
		object = new RObject(true);
		object.oop[0] = classObject;
		classObject.data = classData;
		when(classData.methodAt(MESSAGE)).thenReturn(method);
		RObject.send(object, MESSAGE);
		verify(method).applyTo(object);
	}

	@Test public void shouldLookupMethodInSuperclassOfReceiverWhenNotFoundInReceiver() {
		object = new RObject(true);
		object.oop[0] = classObject;
		classObject.data = classData;
		classObject.oop[1] = superclassObject;
		superclassObject.data = superclassData;
		when(classData.methodAt(MESSAGE)).thenReturn(null);
		when(superclassData.methodAt(MESSAGE)).thenReturn(method);
		RObject.send(object, MESSAGE);
		verify(method).applyTo(object);
	}
}
