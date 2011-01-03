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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import static org.mockito.Mockito.*;

public class GeneratorTest implements Opcodes {

	private static final String CLASS_NAME = "Model";
	private static final String PACKAGE_INTERNAL_NAME = "app/data";
	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = "st/redline/ProtoObject";

	@Mock ClassWriter classWriter;
	@Mock MethodVisitor methodVisitor;

	private Generator generator;

	@Before public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		generator = new Generator(classWriter);
	}

	@Test public void shouldWriteClassAndInitMethodWhenClassOpened() {
		when(classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)).thenReturn(methodVisitor);
		generator.openClass(CLASS_NAME, PACKAGE_INTERNAL_NAME);
		verify(classWriter).visit(V1_5, ACC_PUBLIC + ACC_SUPER, "app/data/Model", null, SUPERCLASS_FULLY_QUALIFIED_NAME, null);
		verify(classWriter).visitSource("Model.st", null);
		verify(methodVisitor).visitCode();
		verifyInvokeOfSuperclassInitMethod();
	}

	@Test public void shouldEndWritingClassAndInitMethodWhenClassClosed() {
		when(classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)).thenReturn(methodVisitor);
		generator.openClass(CLASS_NAME, PACKAGE_INTERNAL_NAME);
		generator.closeClass();
		verifyInitMethodClosed();
		verify(classWriter).visitEnd();
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
