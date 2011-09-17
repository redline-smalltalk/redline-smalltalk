/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.io.File;
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

	private static final String SUPERCLASS = PROTOOBJECT;
	private static final String SEND = "primitiveSend";
	private static final String SUPER_SEND = "primitiveSuperSend";
	private static final String CONSTRUCT = "construct";
	private static final String CONSTRUCT_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_SYMBOL = "primitiveSymbol";
	private static final String PRIMITIVE_SYMBOL_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_INTEGER = "primitiveInteger";
	private static final String PRIMITIVE_INTEGER_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_STRING = "primitiveString";
	private static final String PRIMITIVE_STRING_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_CHARACTER = "primitiveCharacter";
	private static final String PRIMITIVE_CHARACTER_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_VARIABLE_AT = "primitiveVariableAt";
	private static final String PRIMITIVE_VARIABLE_AT_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String[] SEND_SIGNATURES = {
		"(Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
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
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitivePackageRegistryCurrent", "(Ljava/lang/String;)V");
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitLdcInsn("ProtoObject");
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitiveResolveObject", "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitLdcInsn(className + "<Ghost>");
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitiveCreateSubclass", "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;");
		stackDuplicate();
		mv.visitLdcInsn(className);
		mv.visitLdcInsn(makeFullQualifiedPackageName());
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitivePackageAtPut", "(Lst/redline/ProtoObject;Ljava/lang/String;Ljava/lang/String;)V");
		mv.visitInsn(ACONST_NULL);
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, CONSTRUCT, CONSTRUCT_SIGNATURE);
		mv.visitInsn(POP);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitivePackageRegistryRemove", "()V");
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

	public void callPrimitiveVariableAt(String value, int line, boolean loadSide) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_VARIABLE_AT, PRIMITIVE_VARIABLE_AT_SIGNATURE);
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

	public void stackPushClassMethodWasFoundIn() {
		mv.visitVarInsn(ALOAD, 2);
	}

	public void stackPushTrue(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "instanceOfTrue", "Lst/redline/ProtoObject;");
	}

	public void stackPushFalse(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "instanceOfFalse", "Lst/redline/ProtoObject;");
	}

	public void stackPushNil(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "instanceOfUndefinedObject", "Lst/redline/ProtoObject;");
	}

	public void stackPushSmalltalk(int line) {
		visitLine(line);
		mv.visitFieldInsn(GETSTATIC, PROTOOBJECT, "instanceOfSmalltalk", "Lst/redline/ProtoObject;");
	}

	public void stackPushSuper(int line) {
		stackPushReceiver(line);
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
		if (sendToSuper) {
			stackPushClassMethodWasFoundIn();
			mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, SUPER_SEND, SEND_SIGNATURES[argumentCount]);
		} else {
			stackPushNull();
			mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, SEND, SEND_SIGNATURES[argumentCount]);
		}
	}

	public void callPrimitiveSymbol(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_SYMBOL, PRIMITIVE_SYMBOL_SIGNATURE);
	}

	public void callPrimitiveInteger(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_INTEGER, PRIMITIVE_INTEGER_SIGNATURE);
	}
	
	public void callPrimitiveString(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_STRING, PRIMITIVE_STRING_SIGNATURE);
	}

	public void callPrimitiveCharacter(String value, int line) {
		stackPushReceiver(line);
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_CHARACTER, PRIMITIVE_CHARACTER_SIGNATURE);
	}

	public void callClass() {
		mv.visitMethodInsn(INVOKEVIRTUAL, PROTOOBJECT, "cls", "()Lst/redline/ProtoObject;");
	}

	public void callPrimitiveCompileMethod(String fullMethodName, String methodName, String className, String packageName, int countOfArguments) {
		stackPushLiteral(fullMethodName);
		stackPushLiteral(methodName);
		stackPushLiteral(className);
		stackPushLiteral(packageName);
		stackPushNumeric(countOfArguments);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitiveCompileMethod", "(Lst/redline/ProtoObject;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)V");
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
}
