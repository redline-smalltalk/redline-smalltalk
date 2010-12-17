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

import java.io.File;

import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class SmalltalkTest {

	@Mock File sourceFile;
	@Mock Environment environment;

	private Smalltalk smalltalk;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		smalltalk = Smalltalk.with(environment);
	}

	@Test public void shouldNotEvaluateNonExistentFile() {
		when(sourceFile.exists()).thenReturn(false);
		smalltalk.eval(sourceFile);
		verify(environment, never()).put(Smalltalk.CURRENT_FILE, sourceFile);
	}

	@Test public void shouldNotEvaluateNonFile() {
		when(sourceFile.exists()).thenReturn(true);
		when(sourceFile.isFile()).thenReturn(false);
		smalltalk.eval(sourceFile);
		verify(environment, never()).put(Smalltalk.CURRENT_FILE, sourceFile);
	}

	@Test public void shouldTrackSourceFileBeingEvaluated() {
		when(sourceFile.exists()).thenReturn(true);
		when(sourceFile.isFile()).thenReturn(true);
		smalltalk.eval(sourceFile);
		verify(environment).put(Smalltalk.CURRENT_FILE, sourceFile);
		verify(environment).remove(Smalltalk.CURRENT_FILE);
	}

	@Test (expected=MissingArgumentException.class)
	public void shouldNotAcceptNullEnvironmentArgument() {
		Smalltalk.with(null);
	}
}
