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
package st.redline.smalltalk.interpreter;

import java.io.File;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import static org.mockito.Mockito.*;

public class GeneratorTest implements Opcodes {

	private static final String CLASS_NAME = "Model";
	private static final String PACKAGE_INTERNAL_NAME = "app/data";
	private static final String CLASS_FULLY_QUALIFIED_NAME = PACKAGE_INTERNAL_NAME + File.separator + CLASS_NAME;
	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = "st/redline/smalltalk/ProtoObject";
	private static final String UNARY_SELECTOR = "unarySelector";
	private static final String UNARY_METHOD_DESCRIPTOR = "(Lst/redline/smalltalk/ProtoObject;Ljava/lang/String;)Lst/redline/smalltalk/ProtoObject;";
	private static final String SMALLTALK_CLASS = "st/redline/smalltalk/Smalltalk";
	private static final String THREAD_CLASS = "java/lang/Thread";
	private static final int LINE_NUMBER= 42;

	@Mock ClassWriter classWriter;
	@Mock MethodVisitor methodVisitor;

	private Generator generator;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		generator = new Generator(classWriter);
		when(classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)).thenReturn(methodVisitor);
		generator.openClass(CLASS_NAME, PACKAGE_INTERNAL_NAME);
	}

	@Test public void shouldWriteClassAndInitMethodWhenClassOpened() {
		verify(classWriter).visit(V1_5, ACC_PUBLIC + ACC_SUPER, CLASS_FULLY_QUALIFIED_NAME, null, SUPERCLASS_FULLY_QUALIFIED_NAME, null);
		verify(classWriter).visitSource("Model.st", null);
		verify(methodVisitor).visitCode();
		verifyInvokeOfSuperclassInitMethod();
	}

	@Test public void shouldEndWritingClassAndInitMethodWhenClassClosed() {
		generator.closeClass();
		verifyInitMethodClosed();
		verify(classWriter).visitEnd();
	}

	@Test public void shouldGenerateClassLookup() {
		generator.classLookup(CLASS_NAME, LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, THREAD_CLASS, "currentThread", "()Ljava/lang/Thread;");
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, THREAD_CLASS, "getContextClassLoader", "()Ljava/lang/ClassLoader;");
		verify(methodVisitor).visitTypeInsn(CHECKCAST, SMALLTALK_CLASS);
		verify(methodVisitor).visitLdcInsn(CLASS_NAME);
		verify(methodVisitor).visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "basicAt", "(Ljava/lang/String;)Lst/redline/smalltalk/ProtoObject;");
	}

	private void verifyLineNumber(int lineNumber) {
		verify(methodVisitor).visitLabel((Label) notNull());
		verify(methodVisitor).visitLineNumber(eq(lineNumber), (Label) notNull());
	}

	@Test public void shouldGenerateUnarySend() {
		generator.unarySend(UNARY_SELECTOR, LINE_NUMBER);
		verifyLineNumber(LINE_NUMBER);
		verify(methodVisitor).visitLdcInsn(UNARY_SELECTOR);
		verify(methodVisitor).visitMethodInsn(INVOKESTATIC, CLASS_FULLY_QUALIFIED_NAME, "send", UNARY_METHOD_DESCRIPTOR);
	}

	private void verifyInvokeOfSuperclassInitMethod() {
		verify(methodVisitor).visitVarInsn(ALOAD, 0);
		verify(methodVisitor).visitMethodInsn(INVOKESPECIAL, SUPERCLASS_FULLY_QUALIFIED_NAME, "<init>", "()V");
	}

	private void verifyInitMethodClosed() {
		verify(methodVisitor).visitInsn(RETURN);
		verify(methodVisitor).visitMaxs(1, 1);
		verify(methodVisitor).visitEnd();
	}
}
