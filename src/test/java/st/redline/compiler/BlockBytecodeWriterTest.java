/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import st.redline.core.PrimObject;
import st.redline.core.PrimObjectBlock;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;

public class BlockBytecodeWriterTest implements Opcodes {

	static String CLASS_NAME = "Example";
	static String PACKAGE_NAME = "com.domain";

	ClassWriter classWriter;
	MethodVisitor methodVisitor;
	BlockBytecodeWriter writer;
    boolean bootstrapping;

    @Before
	public void setup() {
        bootstrapping = PrimObject.bootstrapping(true);
		methodVisitor = mock(MethodVisitor.class);
		classWriter = mock(ClassWriter.class);
		writer = new BlockBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false, classWriter);
	}

    @After
    public void teardown() {
        PrimObject.bootstrapping(bootstrapping);
    }

    @Test
	public void shouldCreateLoadableClassWhenClassOpenedAndClosed() throws IllegalAccessException, InstantiationException {
		BlockBytecodeWriter writerNotUsingMocks = new BlockBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false);
		writerNotUsingMocks.openClass();
		writerNotUsingMocks.closeClass();
		byte[] classBytes = writerNotUsingMocks.contents();
		PrimObject block = (PrimObject) new TemporaryClassLoader().defineClass(classBytes).newInstance();
		assertNotNull(block);
		assertTrue(block instanceof PrimObjectBlock);
	}

	private class TemporaryClassLoader extends ClassLoader {
		public Class defineClass(byte[] classBytes) {
			return defineClass(null, classBytes, 0, classBytes.length);
		}
	}
}
