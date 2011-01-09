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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.*;

public class ProtoObjectTest {

	private static final String MESSAGE = "example";

	@Mock ProtoObject classObject;
	@Mock ProtoObject superclassObject;
	@Mock ProtoObject protoObject;
	@Mock ProtoMethod protoMethod;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
	}

	@Test public void shouldCreateWithMinimimTwoVariableSlots() {
		protoObject = new ProtoObject();
		assertEquals(2, protoObject.variables.length);
	}

	@Test public void shouldLookupMethodInClassOfReceiver() {
		when(protoObject.basicClass()).thenReturn(classObject);
		when(classObject.basicMethodDictionaryAt(MESSAGE)).thenReturn(protoMethod);
		ProtoObject.send(protoObject, MESSAGE);
		verify(protoMethod).applyTo(protoObject);
	}

	@Test public void shouldLookupMethodInSuperclassOfReceiverWhenNotFoundInReceiver() {
		when(protoObject.basicClass()).thenReturn(classObject);
		when(classObject.basicMethodDictionaryAt(MESSAGE)).thenReturn(null);
		when(classObject.basicSuperclass()).thenReturn(superclassObject);
		when(superclassObject.basicMethodDictionaryAt(MESSAGE)).thenReturn(protoMethod);
		ProtoObject.send(protoObject, MESSAGE);
		verify(protoMethod).applyTo(protoObject);
	}
}
