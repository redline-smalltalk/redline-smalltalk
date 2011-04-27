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
package st.redline.smalltalk.interpreter;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;

public class MethodsTest {

	@Mock NodeVisitor visitor;
	@Mock Method method;
	private Methods methods;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		methods = new Methods();
	}

	@Test public void shouldBeEmptyWhenCreated() {
		assertTrue(methods.isEmpty());
	}

	@Test public void shouldAddMethods() {
		methods.add(method);
		assertFalse(methods.isEmpty());
	}

	@Test public void shouldVisitEachMethod() {
		methods.add(method);
		methods.accept(visitor);
		verify(visitor).visit(methods);
		verify(method).accept(visitor);
	}

	@Test public void shouldEndVisitation() {
		methods.accept(visitor);
		verify(visitor).visitEnd(methods);
	}
}
