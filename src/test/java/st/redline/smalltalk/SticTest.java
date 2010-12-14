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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import st.redline.testsupport.TestThatCapturesSystemOutputs;

import static org.junit.Assert.assertEquals;

public class SticTest extends TestThatCapturesSystemOutputs {

	public static final String LF = System.getProperty("line.separator");

	private static final String[] NO_ARGUMENTS = null;
	private static final String NO_OUTPUT = "";
	private static final String HELP_USAGE =
		"usage: stic" + LF +
		" -?,--help   print this message.";

	@Before
	public void setUp() throws Exception {
		captureSystemOutputs();
	}

	@After
	public void tearDown() throws Exception {
		dontCaptureSystemOutputs();
	}

	@Test
	public void shouldPrintUsageWhenNoArguments() {
		Stic.main(NO_ARGUMENTS);
		assertEquals(HELP_USAGE, capturedStandardOutput());
		assertEquals(NO_OUTPUT, capturedErrorOutput());
	}

	@Test
	public void shouldPrintUsageWhenHelpRequested() {
		Stic.main(new String[] {"-?"});
		assertEquals(HELP_USAGE, capturedStandardOutput());
		assertEquals(NO_OUTPUT, capturedErrorOutput());
	}

	@Test
	public void shouldPrintUsageWhenHelpRequestedWithAlias() {
		Stic.main(new String[] {"--help"});
		assertEquals(HELP_USAGE, capturedStandardOutput());
		assertEquals(NO_OUTPUT, capturedErrorOutput());
	}
}
