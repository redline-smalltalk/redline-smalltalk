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

import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

public class SticTest {

	private static final List<String> listWithOneFileName = new ArrayList<String>();

	@Mock CommandLine commandLine;
	@Mock Smalltalk smalltalk;
	@Mock PrintWriter standardOutput;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		listWithOneFileName.add("somefile.st");
		when(smalltalk.commandLine()).thenReturn(commandLine);
	}

	@Test public void shouldDispatchEvaluationOfFilesToSmalltalk() {
		when(commandLine.helpRequested()).thenReturn(false);
		when(commandLine.haveNoArguments()).thenReturn(false);
		when(commandLine.arguments()).thenReturn(listWithOneFileName);

		new Stic(smalltalk).run();
		verify(smalltalk, times(1)).eval((File) anyObject());
	}

	@Test public void shouldPrintUsageWhenHelpRequested() {
		when(commandLine.helpRequested()).thenReturn(true);
		when(smalltalk.standardOutput()).thenReturn(standardOutput);

		new Stic(smalltalk).run();
		verify(commandLine).printHelp(standardOutput);
	}
}
