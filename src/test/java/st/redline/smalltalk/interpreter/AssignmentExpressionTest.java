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

import static org.mockito.Mockito.verify;

public class AssignmentExpressionTest {

	@Mock NodeVisitor visitor;
	@Mock VariableName variableName;
	@Mock SimpleExpression simpleExpression;
	private AssignmentExpression assignmentExpression;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		assignmentExpression = new AssignmentExpression(variableName, simpleExpression);
	}

	@Test public void shouldIndicateAssignedExpressionsResultIsLeftOnStack() {
		assignmentExpression.leaveResultOnStack();
		verify(simpleExpression).duplicateResultOnStack();
	}

	@Test public void shouldIndicateVariableNameIsLeftOnStoreSideOfExpression() {
		assignmentExpression.accept(visitor);
		verify(variableName).onStoreSideOfExpression();
	}

	@Test public void shouldVisitExpressionAndThenAssignedVariableName() {
		assignmentExpression.accept(visitor);
		verify(simpleExpression).accept(visitor);
		verify(variableName).accept(visitor);
	}
}
