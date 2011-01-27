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
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.File;

public class Generator implements Opcodes {

	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = "st/redline/smalltalk/RObject";
	private static final String SEND_METHOD_NAME = "send";
	private static final String SMALLTALK_CLASS = "st/redline/smalltalk/Smalltalk";
	private static final String[] METHOD_DESCRIPTORS = {
			"(Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
			"(Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Lst/redline/smalltalk/RObject;Ljava/lang/String;)Lst/redline/smalltalk/RObject;",
	};
	private static final int MAXIMUM_KEYWORD_ARGUMENTS = 10;

	private ClassWriter classWriter;
	private String className;
	private String packageInternalName;
	private String fullyQualifiedName;
	private MethodVisitor methodVisitor;
	private boolean traceOn;

	public Generator(boolean traceOn) {
		this.traceOn = traceOn;
	}

	public void initialize() {
		initialize(traceOn ? tracingClassWriter() : nonTracingClassWriter());
	}

	private ClassWriter nonTracingClassWriter() {
		return new ClassWriter(ClassWriter.COMPUTE_MAXS);
	}

	private ClassWriter tracingClassWriter() {
		return new TracingClassWriter(ClassWriter.COMPUTE_MAXS);
	}

	void initialize(ClassWriter classWriter) {
		this.classWriter = classWriter;
	}

	public void openClass(String className, String packageInternalName) {
		rememberNames(className, packageInternalName);
		openClass();
		openInitializeMethod();
	}

	public void openMethodClass() {
	}

	public void closeMethodClass() {
	}

	public void nameMethodClass(String methodClassName) {
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
		this.fullyQualifiedName = packageInternalName.equals("") ? className : packageInternalName + File.separator + className;
	}

	public String fullyQualifiedName() {
		return fullyQualifiedName;
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

	public void classLookup(String className, int line) {
		visitLine(line);
		currentSmalltalkClass();
		methodVisitor.visitLdcInsn(className);
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "primitiveAt", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	private void visitLine(int line) {
		Label label = new Label();
		methodVisitor.visitLabel(label);
		methodVisitor.visitLineNumber(line, label);
	}

	private void currentSmalltalkClass() {
		methodVisitor.visitMethodInsn(INVOKESTATIC, SMALLTALK_CLASS, "instance", "()Lst/redline/smalltalk/Smalltalk;");
	}

	public void unarySend(String unarySelector, int line) {
		visitLine(line);
		methodVisitor.visitLdcInsn(unarySelector);
		methodVisitor.visitMethodInsn(INVOKESTATIC, fullyQualifiedName, SEND_METHOD_NAME, METHOD_DESCRIPTORS[0]);
	}

	public byte[] classBytes() {
		return classWriter.toByteArray();
	}

	public byte[][] methodClassesBytes() {
		return new byte[0][];
	}

	public void stackPop() {
		methodVisitor.visitInsn(POP);
	}

	public void primitiveStringConversion(String string, int line) {
		currentSmalltalkClass();
		methodVisitor.visitLdcInsn(string.substring(1, string.length() - 1));  // remove ''
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "stringFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	public void primitiveSymbolConversion(String symbol, int line) {
		currentSmalltalkClass();
		methodVisitor.visitLdcInsn(symbol);
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, SMALLTALK_CLASS, "symbolFromPrimitive", "(Ljava/lang/String;)Lst/redline/smalltalk/RObject;");
	}

	public void keywordSend(String keywordSelector, int countOfArguments, int line) {
		if (countOfArguments > MAXIMUM_KEYWORD_ARGUMENTS)
			throw new IllegalArgumentException("More than " + MAXIMUM_KEYWORD_ARGUMENTS + " keyword arguments!");
		visitLine(line);
		methodVisitor.visitLdcInsn(keywordSelector);
		methodVisitor.visitMethodInsn(INVOKESTATIC, fullyQualifiedName, SEND_METHOD_NAME, METHOD_DESCRIPTORS[countOfArguments]);
	}

	public void binarySend(String binarySelector, int line) {
		visitLine(line);
		methodVisitor.visitLdcInsn(binarySelector);
		methodVisitor.visitMethodInsn(INVOKESTATIC, fullyQualifiedName, SEND_METHOD_NAME, METHOD_DESCRIPTORS[1]);
	}
}
