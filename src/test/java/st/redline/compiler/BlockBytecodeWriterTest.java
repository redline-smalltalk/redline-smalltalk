/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import st.redline.PrimObject;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class BlockBytecodeWriterTest {

	static String CLASS_NAME = "Example";
	static String PACKAGE_NAME = "com.domain";

	ClassWriter classWriter;
	MethodVisitor methodVisitor;
	BlockBytecodeWriter writer;

	@Before
	public void setup() {
		methodVisitor = mock(MethodVisitor.class);
		classWriter = mock(ClassWriter.class);
		writer = new BlockBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false, classWriter);
	}

	@Test
	public void shouldCreateLoadableClassWhenClassOpenedAndClosed() throws IllegalAccessException, InstantiationException {
		BlockBytecodeWriter writerNotUsingMocks = new BlockBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false);
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
