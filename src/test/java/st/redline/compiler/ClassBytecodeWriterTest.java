/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import com.sun.org.apache.bcel.internal.generic.ALOAD;
import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class ClassBytecodeWriterTest implements Opcodes {

	static String CLASS_NAME = "Example";
	static String PACKAGE_NAME = "com.domain";

	ClassWriter classWriter;
	MethodVisitor methodVisitor;
	ClassBytecodeWriter writer;

	@Before
	public void setup() {
		methodVisitor = mock(MethodVisitor.class);
		classWriter = mock(ClassWriter.class);
		writer = new ClassBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false, classWriter);
	}

	@Test
	public void shouldCreateClassWriterDuringDefaultConstruction() {
		assertNotNull(new ClassBytecodeWriter(CLASS_NAME, PACKAGE_NAME, false).classWriter());
	}

	@Test
	public void shouldPushSlot0AsJavaThis() {
		writer.methodVisitor(methodVisitor);
		writer.pushThis();
		verify(methodVisitor).visitVarInsn(ALOAD, 0);
	}

	@Test
	public void shouldPushSlot1AsSmalltalkReceiver() {
		writer.methodVisitor(methodVisitor);
		writer.pushReceiver();
		verify(methodVisitor).visitVarInsn(ALOAD, 1);
	}

	@Test
	public void shouldPushSlot2AsContext() {
		writer.methodVisitor(methodVisitor);
		writer.pushContext();
		verify(methodVisitor).visitVarInsn(ALOAD, 2);
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
