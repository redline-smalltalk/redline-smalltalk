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
package st.redline.smalltalk;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.PrintWriter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class EnvironmentTest {

	@Mock CommandLine commandLine;
	@Mock PrintWriter standardOutput;
	@Mock PrintWriter errorOutput;

	private Environment environment;
	private String key = "Key";
	private Object value = this;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		environment = Environment.with(commandLine, standardOutput, errorOutput);
	}

	@Test public void shouldStoreKeyedValues() {
		environment.put(key, value);
		assertEquals(this, environment.get(key));
	}

	@Test public void shouldRemoveKeyedValues() {
		environment.put(key, value);
		assertEquals(this, environment.get(key));
		assertEquals(this, environment.remove(key));
		assertFalse(environment.containsKey(key));
	}

	@Test(expected=MissingArgumentException.class)
	public void shouldNotAcceptNullCommandLineArgument() {
		Environment.with(null, standardOutput, errorOutput);
	}

	@Test(expected=MissingArgumentException.class)
	public void shouldNotAcceptNullStandardOutputArgument() {
		Environment.with(commandLine, null, errorOutput);
	}

	@Test(expected=MissingArgumentException.class)
	public void shouldNotAcceptNullErrorOutputArgument() {
		Environment.with(commandLine, standardOutput, null);
	}
}