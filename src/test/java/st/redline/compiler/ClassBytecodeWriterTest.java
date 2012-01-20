/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassWriter;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;

public class ClassBytecodeWriterTest {

	static String CLASS_NAME = "Example";
	static String PACKAGE_NAME = "com.domain";

	ClassWriter classWriter;
	ClassBytecodeWriter writer;

	@Before
	public void setup() {
		classWriter = mock(ClassWriter.class);
		writer = new ClassBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false, classWriter);
	}

	@Test
	public void shouldCreateClassWriterDuringDefaultConstruction() {
		assertNotNull(new ClassBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false).classWriter());
	}

	@Test
	public void shouldCreateLoadableClassWhenClassOpenedAndClosed() throws IllegalAccessException, InstantiationException {
		ClassBytecodeWriter writerNotUsingMocks = new ClassBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false);
		writerNotUsingMocks.openClass();
		writerNotUsingMocks.closeClass();
		byte[] classBytes = writerNotUsingMocks.contents();
		assertNotNull(new TemporaryClassLoader().defineClass(classBytes).newInstance());
	}

	private class TemporaryClassLoader extends ClassLoader {
		public Class defineClass(byte[] classBytes) {
			return defineClass(null, classBytes, 0, classBytes.length);
		}
	}
}
