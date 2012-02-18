package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import st.redline.ClassPathUtilities;

import java.io.PrintWriter;
import java.security.SignatureSpi;

public class ClassBytecodeWriter implements Opcodes {

	private static final String OBJECT = "st/redline/PrimObject";
	private static final String CONTEXT = "st/redline/PrimContext";
	private static final String SEND_MESSAGES = "_sendMessages_";
	private static final String SEND_MESSAGES_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";
	private static final String PRIMITIVE_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";
	private static final String[] SIGNATURES = {
		"(Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;"
	};
	private final String packageName;
	private final boolean verbose;

	protected ClassWriter cw;
	protected MethodVisitor mv;
	private String fullyQualifiedClassName;

	ClassBytecodeWriter(String className, String packageName, boolean verbose) {
		this(className, packageName, verbose, null);
		this.cw = createClassWriter();
	}

	ClassBytecodeWriter(String className, String packageName, boolean verbose, ClassWriter classWriter) {
		this.packageName = packageName;
		this.verbose = verbose;
		this.cw = classWriter;
		fullyQualifiedClassName = ClassPathUtilities.classNameToFullyQualifiedClassName(packageName, className);
	}

	ClassWriter createClassWriter() {
		return verbose ? tracingClassWriter() : nonTracingClassWriter();
	}

	static ClassWriter nonTracingClassWriter() {
		return new ClassWriter(ClassWriter.COMPUTE_MAXS);
	}

	static ClassWriter tracingClassWriter() {
		return new TracingClassWriter(ClassWriter.COMPUTE_MAXS, new PrintWriter(System.out));
	}

	ClassWriter classWriter() {
		return cw;
	}

	byte[] contents() {
		return cw.toByteArray();
	}

	String homogenize(String className) {
		int index = className.indexOf("$");
		if (index == -1)
			return className;
		return className.substring(0, index);
	}

	void methodVisitor(MethodVisitor methodVisitor) {
		mv = methodVisitor;
	}

	void openClass() {
		cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, superclass(), null);
		cw.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
		writeInitializeMethod();
		openMessageSendsMethod();
	}

	void openMessageSendsMethod() {
		mv = cw.visitMethod(ACC_PROTECTED, SEND_MESSAGES, SEND_MESSAGES_SIG, null, null);
		mv.visitCode();
		pushThis();
		pushReceiver();
		pushContext();
		mv.visitMethodInsn(INVOKESPECIAL, superclass(), SEND_MESSAGES, SEND_MESSAGES_SIG);
		pop();
		pushReceiver();
	}

	void writeInitializeMethod() {
		openInitializeMethod();
		registerPackage();
		invokeMessageSends();
		deregisterPackage();
		closeInitializeMethod();
	}

	void deregisterPackage() {
		if (packageName == "")
			return;
		mv.visitFieldInsn(GETSTATIC, OBJECT, "PACKAGE_REGISTRY", "Ljava/lang/ThreadLocal;");
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/ThreadLocal", "get", "()Ljava/lang/Object;");
		mv.visitTypeInsn(CHECKCAST, "java/util/Stack");
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Stack", "pop", "()Ljava/lang/Object;");
		mv.visitInsn(POP);
	}

	void registerPackage() {
		if (packageName == "")
			return;
		mv.visitFieldInsn(GETSTATIC, OBJECT, "PACKAGE_REGISTRY", "Ljava/lang/ThreadLocal;");
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/ThreadLocal", "get", "()Ljava/lang/Object;");
		mv.visitTypeInsn(CHECKCAST, "java/util/Stack");
		pushLiteral(packageName);
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Stack", "push", "(Ljava/lang/Object;)Ljava/lang/Object;");
		mv.visitInsn(POP);
	}

	void invokeMessageSends() {
		pushThis();
		pushThis();
		mv.visitTypeInsn(NEW, "st/redline/PrimContext");
		pushDuplicate();
		pushThis();
		mv.visitMethodInsn(INVOKESPECIAL, "st/redline/PrimContext", "<init>", "(Lst/redline/PrimObject;)V");
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, SEND_MESSAGES, SEND_MESSAGES_SIG);
	}

	String superclass() {
		return "st/redline/PrimObjectMetaclass";
	}

	void closeClass() {
		closeMessageSendsMethod();
		cw.visitEnd();
	}

	void closeMessageSendsMethod() {
		mv.visitInsn(ARETURN);
		closeMethod();
	}

	void closeInitializeMethod() {
		mv.visitInsn(RETURN);
		closeMethod();
	}

	private void closeMethod() {
		mv.visitMaxs(1, 1);
		mv.visitEnd();
	}

	void openInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
		mv.visitCode();
		visitLine(0);
		pushThis();
		mv.visitMethodInsn(INVOKESPECIAL, superclass(), "<init>", "()V");
	}

	void invokeContextTemporariesInit(int size) {
		pushContext();
		pushNumber(size);
		mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporariesInit", "(I)V");
	}

	void invokeObjectPerform(String selector, int argumentCount) {
		pushLiteral(selector);
		mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "perform", SIGNATURES[argumentCount]);
	}

	void invokeObjectCreate(String type, String value, int line) {
		visitLine(line);
		pushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, type, "(Ljava/lang/Object;)Lst/redline/PrimObject;");
	}

	void invokeObjectCompileBlock(String name, int line) {
		visitLine(line);
		pushReceiver();
		pushLiteral(name);
		mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "block", "(Ljava/lang/String;)Lst/redline/PrimObject;");
	}

	void invokeObjectString(String value, int line) {
		invokeObjectCreate("string", value, line);
	}

	void invokeObjectSymbol(String value, int line) {
		invokeObjectCreate("symbol", value, line);
	}

	void invokeObjectCharacter(String value, int line) {
		invokeObjectCreate("character", value, line);
	}

	void invokeObjectNumber(String value, int line) {
		invokeObjectCreate("number", value, line);
	}

	void invokeVariableAt(String name, int line) {
		visitLine(line);
		pushReceiver();
		pushLiteral(name);
		mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "variableAt", "(Ljava/lang/String;)Lst/redline/PrimObject;");
	}

	void invokeVariablePutAt(String name, int line) {
		throw new IllegalStateException("IMPLEMENT ME");
	}

	void invokeObjectArray(int size) {
		pushNumber(size);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "array", "(I)Lst/redline/PrimObject;");
	}

	void invokeArrayPutAt(int index, int line) {
		visitLine(line);
		pushNumber(index);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "putAt", "(Lst/redline/PrimObject;Lst/redline/PrimObject;I)Lst/redline/PrimObject;");
	}

	void invokePrimitive(int line, String primitive) {
		// TODO.JCL - cater for case where primitive fails - for now return primitive result.
		// Doing ARETURN here means there can be more than one ARETURN emitted, this is OK.
		visitLine(line);
		pushThis();
		pushReceiver();
		pushContext();
		mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "p" + primitive, PRIMITIVE_SIG);
		mv.visitInsn(ARETURN);
	}

	void pop() {
		mv.visitInsn(POP);
	}

	void pushArgument(int index) {
		pushContext();
		pushNumber(index);
		mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "argumentAt", "(I)Lst/redline/PrimObject;");
	}

	void pushTemporary(int index) {
		pushContext();
		pushNumber(index);
		mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporaryAt", "(I)Lst/redline/PrimObject;");
	}

	void storeTemporary(int index) {
		pushNumber(index);
		pushContext();
		mv.visitMethodInsn(INVOKESTATIC, CONTEXT, "temporaryPutAtIn", "(Lst/redline/PrimObject;ILst/redline/PrimContext;)V");
	}

	void pushLiteral(String literal) {
		mv.visitLdcInsn(literal);
	}

	void pushDuplicate() {
		mv.visitInsn(DUP);
	}

	void pushThis() {
		mv.visitVarInsn(ALOAD, 0);
	}

	void pushReceiver() {
		mv.visitVarInsn(ALOAD, 1);
	}

	void pushContext() {
		mv.visitVarInsn(ALOAD, 2);
	}

	void pushObjectStaticField(String field) {
		mv.visitFieldInsn(GETSTATIC, OBJECT, field, "Lst/redline/PrimObject;");
	}

	void pushNumber(int value) {
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

	void visitLine(int line) {
		// TODO.jcl uncomment me ASAP.
//		Label l0 = new Label();
//		mv.visitLabel(l0);
//		mv.visitLineNumber(line, l0);
	}
}
