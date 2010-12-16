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

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class SticTest {

	private static final String LF = System.getProperty("line.separator");
	private static final String NO_OUTPUT = "";
	private static final SticCommandLine NO_ARGUMENTS = Stic.commandLineWith(new String[] {});
	private static final SticCommandLine HELP_REQUEST = Stic.commandLineWith(new String[] {"-?"});
	private static final SticCommandLine ALIAS_HELP_REQUEST = Stic.commandLineWith(new String[] {"--help"});

	private static final String HELP_USAGE =
		"usage: stic [options] <source files>" + LF +
		" -?,--help    print this message." + LF;
	private StringWriter outputWriter;
	private StringWriter errorWriter;
	private PrintWriter outputPrintWriter;
	private PrintWriter errorPrintWriter;

	@Before
	public void setUp() throws Exception {
		outputWriter = new StringWriter();
		errorWriter = new StringWriter();
		outputPrintWriter = new PrintWriter(outputWriter);
		errorPrintWriter = new PrintWriter(errorWriter);
	}

	@After
	public void tearDown() throws Exception {}

	@Test
	public void shouldPrintUsageWhenNoArguments() {
		new Stic(NO_ARGUMENTS, outputPrintWriter, errorPrintWriter).run();
		assertHelpOutputWithNoErrors();
	}

	@Test
	public void shouldPrintUsageWhenHelpRequested() {
		new Stic(HELP_REQUEST, outputPrintWriter, errorPrintWriter).run();
		assertHelpOutputWithNoErrors();
	}

	@Test
	public void shouldPrintUsageWhenHelpRequestedWithAlias() {
		new Stic(ALIAS_HELP_REQUEST, outputPrintWriter, errorPrintWriter).run();
		assertHelpOutputWithNoErrors();
	}

	private void assertHelpOutputWithNoErrors() {
		assertEquals(HELP_USAGE, capturedStandardOutput());
		assertEquals(NO_OUTPUT, capturedErrorOutput());
	}

	private String capturedStandardOutput() {
		return contents(outputWriter);
	}

	private String capturedErrorOutput() {
		return contents(errorWriter);
	}

	private String contents(StringWriter outputWriter) {
		return outputWriter.getBuffer().toString();
	}
}
