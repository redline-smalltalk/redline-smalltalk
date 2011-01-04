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

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.File;

public class Generator implements Opcodes {

	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = "st/redline/ProtoObject";
	private static final String SEND_METHOD_NAME = "send";
	private static final String SMALLTALK_CLASS = "st/redline/Smalltalk";
	private static final String[] METHOD_DESCRIPTORS = {
			"(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;"
	};

	private final ClassWriter classWriter;

	private String className;
	private String packageInternalName;
	private String fullyQualifiedName;
	private MethodVisitor methodVisitor;

	public Generator() {
		this(new TracingClassWriter(ClassWriter.COMPUTE_MAXS));
	}

	Generator(ClassWriter classWriter) {
		this.classWriter = classWriter;
	}

	public void openClass(String className, String packageInternalName) {
		rememberNames(className, packageInternalName);
		openClass();
		openInitializeMethod();
	}

	private void openInitializeMethod() {
		methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
		methodVisitor.visitCode();
		invokeSuperclassInitMethod();
	}

	private void invokeSuperclassInitMethod() {
		methodVisitor.visitVarInsn(ALOAD, 0);
		methodVisitor.visitMethodInsn(INVOKESPECIAL, SUPERCLASS_FULLY_QUALIFIED_NAME, "<init>", "()V");
	}

	private void openClass() {
		classWriter.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedName, null, SUPERCLASS_FULLY_QUALIFIED_NAME, null);
		classWriter.visitSource(className + ".st", null);
	}

	private void rememberNames(String className, String packageInternalName) {
		this.className = className;
		this.packageInternalName = packageInternalName;
		this.fullyQualifiedName = packageInternalName + File.separator + className;
	}

	public void closeClass() {
		closeInitializeMethod();
		classWriter.visitEnd();
	}

	private void closeInitializeMethod() {
		methodVisitor.visitInsn(RETURN);
		methodVisitor.visitMaxs(1, 1);
		methodVisitor.visitEnd();
	}

	public void classLookup(String className) {
		currentSmalltalkClass();
		methodVisitor.visitLdcInsn(className);
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "at", "(Ljava/lang/String;)Lst/redline/ProtoObject;");
	}

	private void currentSmalltalkClass() {
		methodVisitor.visitMethodInsn(INVOKESTATIC, "java/lang/Thread", "currentThread", "()Ljava/lang/Thread;");
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Thread", "getContextClassLoader", "()Ljava/lang/ClassLoader;");
		methodVisitor.visitTypeInsn(CHECKCAST, SMALLTALK_CLASS);
	}

	public void unarySend(String unarySelector) {
		methodVisitor.visitLdcInsn(unarySelector);
		methodVisitor.visitMethodInsn(INVOKESTATIC, fullyQualifiedName, SEND_METHOD_NAME, METHOD_DESCRIPTORS[0]);
	}

	public byte[] classBytes() {
		return classWriter.toByteArray();
	}
}
