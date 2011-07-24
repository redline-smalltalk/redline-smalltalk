package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.File;
import java.io.PrintWriter;

public class ClassBytecodeWriter implements Opcodes {

	private static final String REDLINE_OBJECT = "st/redline/RObject";
	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = REDLINE_OBJECT;
	private static final String INIT_METHOD = "<init>";
	private static final String INIT_METHOD_SIGNATURE = "()V";
	private static final String INIT_APPLY_TO_METHOD = "initApplyTo";
	private static final String SEND_METHOD_NAME = "primitiveSend";
	private static final String SUPER_SEND_METHOD_NAME = "primitiveSuperSend";

	private static final String[] SEND_METHOD_DESCRIPTORS = {
		"(Ljava/lang/String;Lst/redline/RObject;)Lst/redline/RObject;",
		"(Lst/redline/RObject;Ljava/lang/String;Lst/redline/RObject;)Lst/redline/RObject;",
	};

	private final String className;
	private String packageName;
	private ClassWriter classWriter;
	private MethodVisitor methodVisitor;
	private String fullyQualifiedClassName;
	private int startingLineNumber;

	public ClassBytecodeWriter(String className, String packageName) {
		this.className = className;
		this.packageName = packageName;
		startingLineNumber = 0;
		initialize();
	}

	private void initialize() {
		classWriter = tracingClassWriter();
		fullyQualifiedClassName = packageName.length() > 0 ? packageName + File.separator + className : className;
	}

	private ClassWriter tracingClassWriter() {
		return new TracingClassWriter(ClassWriter.COMPUTE_MAXS, new PrintWriter(System.out));
	}

	public byte[] contents() {
		return classWriter.toByteArray();
	}

	public void openClass() {
		classWriter.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, SUPERCLASS_FULLY_QUALIFIED_NAME, null);
		classWriter.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
		writeInitializeMethod();
		openApplyToMethod();
	}

	private void openApplyToMethod() {
		methodVisitor = classWriter.visitMethod(ACC_PUBLIC, INIT_APPLY_TO_METHOD, INIT_APPLY_TO_METHOD_SIGNATURE, null, null);
		methodVisitor.visitCode();
	}

	private void writeInitializeMethod() {
		openInitializeMethod();
		invokeSuperclassInitMethod();
		invokeApplyToOnThis();
		closeInitializeMethod();
	}

	private void openInitializeMethod() {
		methodVisitor = classWriter.visitMethod(ACC_PUBLIC, INIT_METHOD, INIT_METHOD_SIGNATURE, null, null);
		methodVisitor.visitCode();
	}

	private void invokeApplyToOnThis() {
		methodVisitor.visitVarInsn(ALOAD, 0);
		methodVisitor.visitVarInsn(ALOAD, 0);
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, INIT_APPLY_TO_METHOD, INIT_APPLY_TO_METHOD_SIGNATURE);
	}

	private void invokeSuperclassInitMethod() {
		methodVisitor.visitVarInsn(ALOAD, 0);
		methodVisitor.visitMethodInsn(INVOKESPECIAL, SUPERCLASS_FULLY_QUALIFIED_NAME, INIT_METHOD, INIT_METHOD_SIGNATURE);
	}

	private String homogenize(String className) {
		int index = className.indexOf("$");
		if (index == -1)
			return className;
		return className.substring(0, index);
	}

	public void closeClass() {
		closeApplyToMethod();
		classWriter.visitEnd();
	}

	private void closeInitializeMethod() {
		stackPop();
		methodVisitor.visitInsn(RETURN);
		methodVisitor.visitMaxs(1, 1);
		methodVisitor.visitEnd();
	}

	private void closeApplyToMethod() {
		methodVisitor.visitInsn(ARETURN);
		methodVisitor.visitMaxs(1, 1);
		methodVisitor.visitEnd();
	}

	private void visitLine(int line) {
		Label label = new Label();
		methodVisitor.visitLabel(label);
		methodVisitor.visitLineNumber(line + startingLineNumber, label);
	}

	public void callPrimitiveVariableAt(String value, int line) {
//		visitLine(line);
		methodVisitor.visitVarInsn(ALOAD, 0);
		methodVisitor.visitLdcInsn(value);
		methodVisitor.visitMethodInsn(INVOKESTATIC, REDLINE_OBJECT, "primitiveVariableAt", "(Lst/redline/RObject;Ljava/lang/String;)Lst/redline/RObject;");
		MethodVisitor mv = methodVisitor;
		mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V");
	}

	public void stackPop() {
		methodVisitor.visitInsn(POP);
	}

	public void stackDuplicate() {
		methodVisitor.visitInsn(DUP);
	}

	public void stackPushNull() {
		methodVisitor.visitInsn(ACONST_NULL);
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
//		visitLine(line);
//		methodVisitor.visitLdcInsn(selector);
//		if (sendToSuper) {
//			methodVisitor.visitVarInsn(ALOAD, 1);  // 0 = receiver, 1 = class method found in.
//			methodVisitor.visitMethodInsn(INVOKEVIRTUAL, REDLINE_OBJECT, SUPER_SEND_METHOD_NAME, SEND_METHOD_DESCRIPTORS[argumentCount]);
//		} else {
//			methodVisitor.visitInsn(ACONST_NULL);
//			methodVisitor.visitMethodInsn(INVOKEVIRTUAL, REDLINE_OBJECT, SEND_METHOD_NAME, SEND_METHOD_DESCRIPTORS[argumentCount]);
//		}
	}
}
