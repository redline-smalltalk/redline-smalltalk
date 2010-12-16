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

import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CommandLineTest {

	private static final String LF = System.getProperty("line.separator");
	private static final String[] NO_ARGUMENTS = new String[] {};
	private static final String[] HELP_IN_ARGUMENTS = new String[] {"-?"};
	private static final String[] HELP_ALIAS_IN_ARGUMENTS = new String[] {"--help"};
	private static final String[] TWO_SOURCE_FILE_NAMES_IN_ARGUMENTS = new String[] {"foo.st", "bar.st"};
	private static final String EXPECTED_HELP_MESSAGE =
									"usage: stic [options] <source files>" + LF +
									" -?,--help    print this message." + LF;

	@Test public void shouldPrintHelpMessage() {
		StringWriter stringWriter = new StringWriter();
		new CommandLine(null).printHelp(new PrintWriter(stringWriter));
		assertEquals(EXPECTED_HELP_MESSAGE, stringWriter.getBuffer().toString());
	}

	@Test public void shouldHaveEmptyArgumentsWhenNullArguments() {
		CommandLine commandLine = new CommandLine(null);
		assertTrue(commandLine.arguments().isEmpty());
	}

	@Test public void shouldHaveEmptyArgumentsWhenNoArguments() {
		CommandLine commandLine = new CommandLine(NO_ARGUMENTS);
		assertTrue(commandLine.arguments().isEmpty());
		assertTrue(commandLine.haveNoArguments());
	}

	@Test public void shouldDefaultToHelpRequestedWhenNoArguments() {
		CommandLine commandLine = new CommandLine(NO_ARGUMENTS);
		assertTrue(commandLine.helpRequested());
		assertTrue(commandLine.haveNoArguments());
		assertFalse(commandLine.haveHelpOption());
	}

	@Test public void shouldHaveHelpRequestedWhenHelpOptionInArguments() {
		CommandLine commandLine = new CommandLine(HELP_IN_ARGUMENTS);
		assertTrue(commandLine.helpRequested());
		assertTrue(commandLine.haveHelpOption());
		assertTrue(commandLine.haveNoArguments());
	}

	@Test public void shouldHaveHelpRequestedWhenHelpAliasOptionInArguments() {
		CommandLine commandLine = new CommandLine(HELP_ALIAS_IN_ARGUMENTS);
		assertTrue(commandLine.helpRequested());
		assertTrue(commandLine.haveHelpOption());
		assertTrue(commandLine.haveNoArguments());
	}

	@Test public void shouldHaveArgumentsWhenSourceFileNamesInArguments() {
		CommandLine commandLine = new CommandLine(TWO_SOURCE_FILE_NAMES_IN_ARGUMENTS);
		assertFalse(commandLine.arguments().isEmpty());
		assertFalse(commandLine.haveNoArguments());
		assertEquals(2, commandLine.arguments().size());
	}
}