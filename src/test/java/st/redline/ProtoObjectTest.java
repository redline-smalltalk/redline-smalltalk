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
package st.redline;

import org.junit.*;
import org.junit.runner.RunWith;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import static org.easymock.EasyMock.expect;
import static org.powermock.api.easymock.PowerMock.*;

@RunWith(PowerMockRunner.class)
@PrepareForTest( { ProtoObject.class })
public class ProtoObjectTest {
	@Test
	public void integerAddition() {
		// we're going to ensure that adding up 1 and 2 works.
		ProtoObject integer1 = createMock(ProtoObject.class);
		ProtoObject integer2 = createMock(ProtoObject.class);

		// integer1 is our receiver;
		expect(integer1.javaValue()).andReturn(new Integer(1));
		// integer2 is our argument;
		expect(integer2.javaValue()).andReturn(new Integer(2));

		mockStaticPartial(ProtoObject.class, "primitiveInteger");
		ProtoObject result = createMock(ProtoObject.class);
		// this call to primitiveInteger generates our result, which is 1 + 2
		expect(ProtoObject.primitiveInteger(integer1, 1 + 2)).andReturn(result);

		replay(integer1, integer2, result, ProtoObject.class);

		ProtoObject.primitive_21(integer1, null, integer2, null, null, null, null);

		// verify that the expected, mocked partial (primitiveInteger) is called
		verify(ProtoObject.class);
	}
}
