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

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.*;

public class ProtoObjectTest {

	private static final String MESSAGE = "example";

	@Mock ProtoMethod protoMethod;
	@Mock ProtoData classProtoData;
	@Mock ProtoData superclassProtoData;

	private ProtoObject protoObject;
	private ProtoObject classObject;
	private ProtoObject superclassObject;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		classObject = new ProtoObject(true);
		superclassObject = new ProtoObject(true);
	}

	@Test public void shouldCreateBasicClass() {
		protoObject = new ProtoObject(true);
		assertEquals(2, protoObject.oop.length);
		assertNotNull(protoObject.data);
		assertNull(protoObject.data.methodAt("nonExistentSelector"));
	}

	@Test public void shouldCreateBasicInstance() {
		protoObject = new ProtoObject(false);
		assertEquals(1, protoObject.oop.length);
		assertNotNull(protoObject.data);
	}

	@Test (expected=IllegalStateException.class)
	public void shouldNotHaveMethodDictionaryInInstance() {
		protoObject = new ProtoObject(false);
		protoObject.data.methodAt("anySelector");
	}

	@Test public void shouldLookupMethodInClassOfReceiver() {
		protoObject = new ProtoObject(true);
		protoObject.oop[0] = classObject;
		classObject.data = classProtoData;
		when(classProtoData.methodAt(MESSAGE)).thenReturn(protoMethod);
		ProtoObject.send(protoObject, MESSAGE);
		verify(protoMethod).applyTo(protoObject);
	}

	@Test public void shouldLookupMethodInSuperclassOfReceiverWhenNotFoundInReceiver() {
		protoObject = new ProtoObject(true);
		protoObject.oop[0] = classObject;
		classObject.data = classProtoData;
		classObject.oop[1] = superclassObject;
		superclassObject.data = superclassProtoData;
		when(classProtoData.methodAt(MESSAGE)).thenReturn(null);
		when(superclassProtoData.methodAt(MESSAGE)).thenReturn(protoMethod);
		ProtoObject.send(protoObject, MESSAGE);
		verify(protoMethod).applyTo(protoObject);
	}
}
