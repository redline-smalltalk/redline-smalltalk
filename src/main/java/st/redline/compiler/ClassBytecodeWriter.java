package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import st.redline.ClassPathUtilities;

import java.io.PrintWriter;

public class ClassBytecodeWriter implements Opcodes {

	private static final String CONTEXT = "st/redline/PrimContext";
	private static final String SEND_MESSAGES = "_sendMessages_";
	private static final String SEND_MESSAGES_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";

	private final boolean verbose;
	private ClassWriter cw;
	private MethodVisitor mv;
	private String fullyQualifiedClassName;

	ClassBytecodeWriter(String className, String packageName, boolean verbose) {
		this(className, packageName, verbose, null);
		this.cw = createClassWriter();
	}

	ClassBytecodeWriter(String className, String packageName, boolean verbose, ClassWriter classWriter) {
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
	}

	void writeInitializeMethod() {
		openInitializeMethod();
		invokeMessageSends();
		closeInitializeMethod();
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

	void pop() {
		mv.visitInsn(POP);
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
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(line, l0);
	}
}
