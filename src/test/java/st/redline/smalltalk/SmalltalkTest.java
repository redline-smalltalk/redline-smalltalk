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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import st.redline.smalltalk.interpreter.Interpreter;

import java.io.File;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class SmalltalkTest {

	String sourceCode = "";

	@Mock File sourceFile;
	@Mock FileReader fileReader;
	@Mock Environment environment;
	@Mock
	Interpreter interpreter;

	private Smalltalk smalltalk;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
	}

	@Test public void shouldInterpretSourceCode() {
		when(environment.get(Smalltalk.INTERPRETER)).thenReturn(interpreter);
		smalltalk = Smalltalk.with(environment);
		smalltalk.eval(sourceCode);
		verify(interpreter).interpretUsing(sourceCode, smalltalk);
	}

	@Test public void shouldProvideAccessToEnvironment() {
		smalltalk = Smalltalk.with(environment);
		assertEquals(environment, smalltalk.environment());
	}

	@Test public void shouldInitializeInterpreterWhenNotSupplied() {
		when(environment.get(Smalltalk.INTERPRETER)).thenReturn(null);
		Smalltalk.with(environment);
		verify(environment).put(eq(Smalltalk.INTERPRETER), isNotNull());
	}

	// Users can supply their own Interpreter.
	@Test public void shouldNotInitializeInterpreterWhenSupplied() {
		when(environment.get(Smalltalk.INTERPRETER)).thenReturn(interpreter);
		Smalltalk.with(environment);
		verify(environment, never()).put(eq(Smalltalk.INTERPRETER), isNotNull());
	}

	@Test public void shouldInitializeFileReaderWhenNotSupplied() {
		when(environment.get(Smalltalk.FILE_READER)).thenReturn(null);
		Smalltalk.with(environment);
		verify(environment).put(eq(Smalltalk.FILE_READER), isNotNull());
	}

	// Users can supply their own File Reader object.
	@Test public void shouldNotInitializeFileReaderWhenSupplied() {
		when(environment.get(Smalltalk.FILE_READER)).thenReturn(fileReader);
		Smalltalk.with(environment);
		verify(environment, never()).put(eq(Smalltalk.FILE_READER), isNotNull());
	}

	// Users can see which file is being evaluated via the Environment.
	@Test public void shouldTrackSourceFileBeingEvaluated() {
		when(environment.get(Smalltalk.INTERPRETER)).thenReturn(interpreter);
		when(environment.get(Smalltalk.FILE_READER)).thenReturn(fileReader);
		when(fileReader.read(sourceFile)).thenReturn("");
		Smalltalk.with(environment).eval(sourceFile);
		verify(environment).put(Smalltalk.CURRENT_FILE, sourceFile);
		verify(environment).remove(Smalltalk.CURRENT_FILE);
	}

	@Test (expected=MissingArgumentException.class)
	public void shouldNotAcceptNullEnvironmentArgument() {
		Smalltalk.with(null);
	}
}
