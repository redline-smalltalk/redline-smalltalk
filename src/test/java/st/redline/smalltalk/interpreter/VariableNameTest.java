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
package st.redline.smalltalk.interpreter;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;

public class VariableNameTest {

	@Mock NodeVisitor visitor;
	private VariableName variableName;
	private VariableName className;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		variableName = new VariableName("var", 10);
		className = new VariableName("Object", 10);
	}

	@Test public void shouldVisitVariableName() {
		variableName.accept(visitor);
		verify(visitor).visit(variableName, "var", 10);
	}

	@Test public void shouldBeOnLoadSideOfExpressionWhenCreated() {
		assertTrue(variableName.isOnLoadSideOfExpression());
	}

	@Test public void shouldBeOnStoreSideOfExpressionWhenIndicated() {
		variableName.onStoreSideOfExpression();
		assertFalse(variableName.isOnLoadSideOfExpression());
	}

	@Test public void shouldKnowWhenClassReference() {
		assertFalse(variableName.isClassReference());
		assertTrue(className.isClassReference());
	}

	@Test public void shouldBeIndexable() {
		assertEquals(variableName.index, 0);
	}
}
