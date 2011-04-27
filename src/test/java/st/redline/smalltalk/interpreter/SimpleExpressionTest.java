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
import static org.mockito.Mockito.verify;

public class SimpleExpressionTest {

	@Mock NodeVisitor visitor;
	@Mock Primary primary;
	@Mock MessageExpression messageExpression;
	@Mock MessageElement messageElement;
	private SimpleExpression simpleExpression;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		simpleExpression = new SimpleExpression();
		simpleExpression.add(primary);
	}

	@Test public void shouldDefaultToNotLeavingResultOnStack() {
		assertFalse(simpleExpression.isResultLeftOnStack());
	}

	@Test public void shouldDefaultToNotDuplicatingResultOnStack() {
		assertFalse(simpleExpression.isResultDuplicatedOnStack());
	}

	@Test public void shouldVisitPrimary() {
		simpleExpression.accept(visitor);
		verify(primary).accept(visitor);
	}

	@Test public void shouldVisitMessageExpression() {
		simpleExpression.add(messageExpression);
		simpleExpression.accept(visitor);
		verify(messageExpression).accept(visitor);
	}

	@Test public void shouldVisitMessageElements() {
		simpleExpression.add(messageElement);
		simpleExpression.accept(visitor);
		verify(messageElement).accept(visitor);
	}

	@Test public void shouldEndVisitation() {
		simpleExpression.accept(visitor);
		verify(visitor).visitEnd(simpleExpression);
	}
}
