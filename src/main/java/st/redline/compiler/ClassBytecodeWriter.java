/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.io.PrintWriter;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import st.redline.ClassPathUtilities;
import st.redline.SmalltalkClassLoader;

public class ClassBytecodeWriter implements Opcodes {

	protected static final String INIT = "<init>";
	protected static final String INIT_SIGNATURE = "()V";
	protected static final String PROTOOBJECT = "st/redline/ProtoObject";
	protected static final String PRIMITIVE = "st/redline/Primitives";

	private static final String SUPERCLASS = PROTOOBJECT;
	private static final String SEND = "send";
	private static final String SUPER_SEND = "superSend";
	private static final String CONSTRUCT = "construct";
	private static final String CONSTRUCT_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ThisContext;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_ARRAY = "createArray";
	private static final String PRIMITIVE_ARRAY_SIGNATURE = "(Lst/redline/ProtoObject;I)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_SYMBOL = "createSymbol";
	private static final String PRIMITIVE_SYMBOL_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_INTEGER = "createInteger";
	private static final String PRIMITIVE_INTEGER_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_STRING = "createString";
	private static final String PRIMITIVE_STRING_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_CHARACTER = "createCharacter";
	private static final String PRIMITIVE_CHARACTER_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_VARIABLE_AT = "variableAt";
	private static final String PRIMITIVE_VARIABLE_AT_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;Z)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_VARIABLE_PUT_AT = "variablePutAt";
	private static final String PRIMITIVE_VARIABLE_PUT_AT_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;Z)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_TEMPORARY_AT = "temporaryAt";
	private static final String PRIMITIVE_TEMPORARY_AT_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ThisContext;IZ)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_TEMPORARY_PUT_AT = "temporaryPutAt";
	private static final String PRIMITIVE_TEMPORARY_PUT_AT_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ThisContext;IZ)V";
	private static final String PRIMITIVE_TEMPORARIES_INIT = "temporariesInit";
	private static final String PRIMITIVE_TEMPORARIES_INIT_SIGNATURE = "(Lst/redline/ThisContext;I)V";
	private static final String PRIMITIVE_PUT_AT = "putAt";
	private static final String PRIMITIVE_PUT_AT_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ProtoObject;I)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_OUTERCONTEXT_METHOD_ARGUMENT_AT = "outerContextMethodArgumentAt";
	private static final String PRIMITIVE_OUTERCONTEXT_METHOD_ARGUMENT_AT_SIGNATURE = "(Lst/redline/ProtoObject;I)Lst/redline/ProtoObject;";
	private static final String[] SEND_SIGNATURES = {
		"(Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ThisContext;)Lst/redline/ProtoObject;",
	};

	protected final String className;

	private String packageName;
	private String fullyQualifiedClassName;
	private int currentLine = 0;

	protected ClassWriter cw;
	protected MethodVisitor mv;

	public ClassBytecodeWriter(String className, String packageName) {
		this.className = className;
		this.packageName = packageName;
		initialize();
	}

	private void initialize() {
		cw = verbose() ? tracingClassWriter() : nonTracingClassWriter();
		fullyQualifiedClassName = ClassPathUtilities.classNameToFullyQualifiedClassName(packageName, className);
	}

	private boolean verbose() {
		return SmalltalkClassLoader.instance().commandLine().verboseRequested();
	}

	private ClassWriter nonTracingClassWriter() {
		return new ClassWriter(ClassWriter.COMPUTE_MAXS);
	}

	private ClassWriter tracingClassWriter() {
		return new TracingClassWriter(ClassWriter.COMPUTE_MAXS, new PrintWriter(System.out));
	}

	public MethodVisitor methodVisitor() {
		return mv;
	}

	public byte[] contents() {
		return cw.toByteArray();
	}

	public void openClass() {
		cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, superclass(), null);
		cw.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
		writeInitializeMethod();
		openApplyToMethod();
	}

	protected String superclass() {
		return SUPERCLASS;
	}

	protected void openApplyToMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, CONSTRUCT, CONSTRUCT_SIGNATURE, null, null);
		mv.visitCode();
	}

	protected void writeInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, INIT, INIT_SIGNATURE, null, null);
		mv.visitCode();
		stackPushThis();
		mv.visitMethodInsn(INVOKESPECIAL, SUPERCLASS, INIT, INIT_SIGNATURE);
		stackPushThis();
		mv.visitLdcInsn(className + "<Loader>");
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, "name", "(Ljava/lang/String;)V");
		stackPushLiteral(packageName);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "packageRegistryCurrent", "(Ljava/lang/String;)V");
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitLdcInsn("ProtoObject");
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "resolveObject", "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitLdcInsn(className + "<Eigenclass>");
		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "createEigenSubclass", "(Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;");
		stackDuplicate();
		mv.visitLdcInsn(className);
		mv.visitLdcInsn(makeFullQualifiedPackageName());
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "packageAtPut", "(Lst/redline/ProtoObject;Ljava/lang/String;Ljava/lang/String;)V");
		mv.visitTypeInsn(NEW, "st/redline/ThisContext");
		mv.visitInsn(DUP);
		mv.visitMethodInsn(INVOKESPECIAL, "st/redline/ThisContext", "<init>", "()V");
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, CONSTRUCT, CONSTRUCT_SIGNATURE);
		mv.visitInsn(POP);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "initialiseNewClasses", "()V");
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "packageRegistryRemove", "()V");
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();
	}

	private String makeFullQualifiedPackageName() {
		return fullyQualifiedClassName;
	}

	private String homogenize(String className) {
		int index = className.indexOf("$");
		if (index == -1)
			return className;
		return className.substring(0, index);
	}

	public void closeClass() {
		closeApplyToMethod();
		cw.visitEnd();
	}

	private void closeApplyToMethod() {
		mv.visitInsn(ARETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();
	}

	protected void visitLine(int line) {
		// Note: we don't visit the same line repeatedly. Is this and ok debug experience? Need to check with java debugger.
		if (line == currentLine)
			return;
		currentLine = line;
		Label label = new Label();
		mv.visitLabel(label);
		mv.visitLineNumber(line, label);
	}

	public void callPrimitiveInitializeTemporaries(int line, int size) {
		visitLine(line);
		stackPushThisContext();
		stackPushNumeric(size);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_TEMPORARIES_INIT, PRIMITIVE_TEMPORARIES_INIT_SIGNATURE);
	}

	public void callPrimitiveTemporaryAt(String value, int line, int index, boolean isLocal) {
		stackPushReceiver(line);
		stackPushThisContext();
		stackPushNumeric(index);
		stackPushBoolean(isLocal);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_TEMPORARY_AT, PRIMITIVE_TEMPORARY_AT_SIGNATURE);
	}

	public void callPrimitiveTemporaryPutAt(String value, int line, int index, boolean isLocal) {
		stackPushReceiver(line);
		stackPushThisContext();
		stackPushNumeric(index);
		stackPushBoolean(isLocal);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_TEMPORARY_PUT_AT, PRIMITIVE_TEMPORARY_PUT_AT_SIGNATURE);
	}

	public void callPrimitiveVariableAt(String value, int line, boolean isClassMethod) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		stackPushBoolean(isClassMethod);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_VARIABLE_AT, PRIMITIVE_VARIABLE_AT_SIGNATURE);
	}

	public void callPrimitiveVariablePutAt(String value, int line, boolean isClassMethod) {
		visitLine(line);
		stackPushLiteral(value);
		stackPushReceiver(line);
		stackPushBoolean(isClassMethod);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_VARIABLE_PUT_AT, PRIMITIVE_VARIABLE_PUT_AT_SIGNATURE);
	}

	public void callOuterContextMethodArgumentAt(int methodArgumentIndex, int line) {
		stackPushReceiver(line);
		stackPushNumeric(methodArgumentIndex);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_OUTERCONTEXT_METHOD_ARGUMENT_AT, PRIMITIVE_OUTERCONTEXT_METHOD_ARGUMENT_AT_SIGNATURE);
	}

	public void stackPushLiteral(String value) {
		mv.visitLdcInsn(value);
	}

	public void stackPop() {
		mv.visitInsn(POP);
	}

	public void stackDuplicate() {
		mv.visitInsn(DUP);
	}

	public void stackPushNull() {
		mv.visitInsn(ACONST_NULL);
	}

	public void stackPushThis() {
		mv.visitVarInsn(ALOAD, 0); // 0 = this, 1 = receiver, 2 = class method found in.
	}

	public void stackPushReceiver(int line) {
		visitLine(line);
		mv.visitVarInsn(ALOAD, 1);
	}

	public void stackPushLocal(int index) {
		mv.visitVarInsn(ALOAD, index);
	}

	public void stackPushThisContext() {
		mv.visitVarInsn(ALOAD, 2);
	}

	public void stackPushClassMethodWasFoundIn() {
		mv.visitVarInsn(ALOAD, 2);
		mv.visitFieldInsn(GETFIELD, "Lst/redline/ThisContext;", "classMethodFoundIn", "Lst/redline/ProtoObject;");
	}

	public void stackPushTrue(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "TRUE", "Lst/redline/ProtoObject;");
	}

	public void stackPushFalse(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "FALSE", "Lst/redline/ProtoObject;");
	}

	public void stackPushNil(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "NIL", "Lst/redline/ProtoObject;");
	}

	public void stackPushSmalltalk(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "SMALLTALK", "Lst/redline/ProtoObject;");
	}

	public void stackPushSuper(int line) {
		stackPushReceiver(line);
	}

	public void stackPushBoolean(boolean bool) {
		mv.visitInsn(bool ? ICONST_1 : ICONST_0);
	}

	public void stackPushNumeric(int value) {
		switch (value) {
			case 0: mv.visitInsn(ICONST_0); break;
			case 1: mv.visitInsn(ICONST_1); break;
			case 2: mv.visitInsn(ICONST_2); break;
			case 3: mv.visitInsn(ICONST_3); break;
			case 4: mv.visitInsn(ICONST_4); break;
			case 5: mv.visitInsn(ICONST_5); break;
			default:
				if (value > 5 && value < 128)
					mv.visitIntInsn(BIPUSH, value);
				else // SIPUSH not supported yet.
					throw new IllegalStateException("push of integer value " + value + " not yet supported.");
		}
	}

	public void unarySend(String unarySelector, int line, boolean sendToSuper) {
		callPrimitiveSend(unarySelector, 0, line, sendToSuper);
	}

	public void binarySend(String binarySelector, int line, boolean sendToSuper) {
		callPrimitiveSend(binarySelector, 1, line, sendToSuper);
	}

	public void keywordSend(String keywords, int argumentCount, int line, boolean sendToSuper) {
		callPrimitiveSend(keywords, argumentCount, line, sendToSuper);
	}

	private void callPrimitiveSend(String selector, int argumentCount, int line, boolean sendToSuper) {
		visitLine(line);
		stackPushLiteral(selector);
		stackPushThisContext();
		if (sendToSuper)
			mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, SUPER_SEND, SEND_SIGNATURES[argumentCount]);
		else
			mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, SEND, SEND_SIGNATURES[argumentCount]);
	}

	public void callPrimitiveSymbol(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_SYMBOL, PRIMITIVE_SYMBOL_SIGNATURE);
	}

	public void callPrimitiveInteger(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_INTEGER, PRIMITIVE_INTEGER_SIGNATURE);
	}

	public void callPrimitiveInteger(int value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(String.valueOf(value));
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_INTEGER, PRIMITIVE_INTEGER_SIGNATURE);
	}

	public void callPrimitiveString(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_STRING, PRIMITIVE_STRING_SIGNATURE);
	}

	public void callPrimitiveArray(int size, int line) {
		stackPushReceiver(line);
		stackPushNumeric(size);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_ARRAY, PRIMITIVE_ARRAY_SIGNATURE);
	}

	public void callPrimitiveCharacter(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_CHARACTER, PRIMITIVE_CHARACTER_SIGNATURE);
	}

	public void callClass() {
		mv.visitMethodInsn(INVOKEVIRTUAL, PROTOOBJECT, "cls", "()Lst/redline/ProtoObject;");
	}

	public void callPrimitiveCompileMethod(String fullMethodName, String methodName, String className, String packageName, int countOfArguments, boolean isClassMethod) {
		stackPushLiteral(fullMethodName);
		stackPushLiteral(methodName);
		stackPushLiteral(className);
		stackPushLiteral(packageName);
		stackPushNumeric(countOfArguments);
		stackPushBoolean(isClassMethod);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "compileMethod", "(Lst/redline/ProtoObject;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;IZ)V");
	}

	public void callPrimitiveCompileBlock(String fullBlockName, int line, String blockName, String className, String packageName, int countOfArguments, boolean isClassMethod) {
		stackPushReceiver(line);
		stackPushLiteral(fullBlockName);
		stackPushLiteral(blockName);
		stackPushLiteral(className);
		stackPushLiteral(packageName);
		stackPushNumeric(countOfArguments);
		stackPushBoolean(isClassMethod);
		stackPushThisContext();
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, "compileBlock", "(Lst/redline/ProtoObject;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;IZLst/redline/ThisContext;)Lst/redline/ProtoBlock;");
	}

	public void callToPrimitiveByNumber(int methodArgumentCount, int methodTemporariesCount, String primitive, int line) {
		throw new IllegalStateException("Can't call primitives from outside of a method.");
	}

	public void loadJavaValue(int line) {
		stackPushReceiver(line);
		mv.visitMethodInsn(INVOKEVIRTUAL, PROTOOBJECT, "javaValue", "()Ljava/lang/Object;");
	}

	public void storeJavaValue(int line) {
		stackPushReceiver(line);
		mv.visitMethodInsn(INVOKEVIRTUAL, PROTOOBJECT, "javaValue", "(Ljava/lang/Object;)V");
	}

	public void callPrimitivePutAt(int index, int line) {
		visitLine(line);
		stackPushNumeric(index);
		mv.visitMethodInsn(INVOKESTATIC, PRIMITIVE, PRIMITIVE_PUT_AT, PRIMITIVE_PUT_AT_SIGNATURE);
	}
}
