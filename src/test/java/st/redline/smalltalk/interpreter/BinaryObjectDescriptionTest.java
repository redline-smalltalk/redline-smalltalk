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

import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class BinaryObjectDescriptionTest {

	@Mock NodeVisitor visitor;
	@Mock Primary primary;
	@Mock UnarySelector unarySelector;
	@Mock BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription;
	private BinaryObjectDescription binaryObjectDescription;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		binaryObjectDescription = new BinaryObjectDescription(primary);
	}

	@Test public void shouldVisitBinaryObjectExpression() {
		binaryObjectDescription.accept(visitor);
		verify(visitor).visit(binaryObjectDescription);
	}

	@Test public void shouldVisitPrimary() {
		binaryObjectDescription.accept(visitor);
		verify(primary).accept(visitor);
	}

	@Test public void shouldVisitUnarySelectors() {
		binaryObjectDescription.add(unarySelector);
		binaryObjectDescription.accept(visitor);
		verify(unarySelector).accept(visitor);
	}

	@Test public void shouldVisitBinarySelectorUnaryObjectDescriptions() {
		binaryObjectDescription.add(binarySelectorUnaryObjectDescription);
		binaryObjectDescription.accept(visitor);
		verify(binarySelectorUnaryObjectDescription).accept(visitor);
	}

	@Test public void shouldConvertPrimaryToVariableNameList() {
		when(primary.line()).thenReturn(32);
		when(primary.value()).thenReturn("instanceVariable");
		List<InstanceVariableName> variableNames = binaryObjectDescription.toInstanceVariableNames();
		assertEquals(variableNames.size(), 1);
		assertEquals(variableNames.get(0).line, 32);
		assertEquals(variableNames.get(0).value, "instanceVariable");
	}
}
