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

public class UnaryExpressionTest {

	@Mock NodeVisitor visitor;
	@Mock UnarySelector unarySelector;
	@Mock BinaryExpression binaryExpression;
	@Mock KeywordExpression keywordExpression;
	private UnaryExpression unaryExpression;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		unaryExpression = new UnaryExpression();
		unaryExpression.add(unarySelector);
	}

	@Test public void shouldVisitEachUnarySelector() {
		unaryExpression.accept(visitor);
		verify(unarySelector).accept(visitor);
	}

	@Test public void shouldVisitBinaryExpression() {
		unaryExpression.add(binaryExpression);
		unaryExpression.accept(visitor);
		verify(binaryExpression).accept(visitor);
	}

	@Test public void shouldVisitKeywordExpression() {
		unaryExpression.add(keywordExpression);
		unaryExpression.accept(visitor);
		verify(keywordExpression).accept(visitor);
	}
}
