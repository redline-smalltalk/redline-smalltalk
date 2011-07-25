package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.File;
import java.io.PrintWriter;

public class ClassBytecodeWriter implements Opcodes {

	private static final String PROTOOBJECT = "st/redline/ProtoObject";
	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = PROTOOBJECT;
	private static final String SEND_METHOD_NAME = "primitiveSend";
	private static final String SUPER_SEND_METHOD_NAME = "primitiveSuperSend";
	private static final String CONSTRUCT_METHOD_NAME = "construct";
	private static final String CONSTRUCTION_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_SYMBOL_METHOD_NAME = "primitiveSymbol";
	private static final String PRIMITIVE_SYMBOL_SIGNATURE = "(Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String INIT_SIGNATURE = "<init>";

	private static final String[] SEND_METHOD_DESCRIPTORS = {
		"(Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
	};
	private final String className;
	private String packageName;
	private ClassWriter cw;
	private MethodVisitor mv;
	private String fullyQualifiedClassName;

	public ClassBytecodeWriter(String className, String packageName) {
		this.className = className;
		this.packageName = packageName;
		initialize();
	}

	private void initialize() {
		cw = tracingClassWriter();
		fullyQualifiedClassName = packageName.length() > 0 ? packageName + File.separator + className : className;
	}

	private ClassWriter tracingClassWriter() {
		return new TracingClassWriter(ClassWriter.COMPUTE_MAXS, new PrintWriter(System.out));
	}

	public byte[] contents() {
		return cw.toByteArray();
	}

	public void openClass() {
		cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, SUPERCLASS_FULLY_QUALIFIED_NAME, null);
		cw.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
		writeInitializeMethod();
		openApplyToMethod();
	}

	private void openApplyToMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, CONSTRUCT_METHOD_NAME, CONSTRUCTION_SIGNATURE, null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);  // TODO.JCL **THIS NEEDS TO BE REMOVED**
	}

	private void writeInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, INIT_SIGNATURE, "()V", null, null);
		mv.visitCode();
		stackPushThis();
		mv.visitMethodInsn(INVOKESPECIAL, SUPERCLASS_FULLY_QUALIFIED_NAME, INIT_SIGNATURE, "()V");
		stackPushThis();
		stackPushThis();
		stackPushThis();
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, CONSTRUCT_METHOD_NAME, CONSTRUCTION_SIGNATURE);
		mv.visitInsn(POP);
		mv.visitInsn(RETURN);
		mv.visitMaxs(2, 1);
		mv.visitEnd();
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

	private void visitLine(int line) {
		Label label = new Label();
		mv.visitLabel(label);
		mv.visitLineNumber(line, label);
	}

	public void callPrimitiveVariableAt(String value, int line) {
		visitLine(line);
		stackPushReceiver();
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitiveVariableAt", "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;");
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

	public void stackPushReceiver() {
		mv.visitVarInsn(ALOAD, 1);
	}

	public void stackPushClassMethodWasFoundIn() {
		mv.visitVarInsn(ALOAD, 2);
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
			mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, SUPER_SEND_METHOD_NAME, SEND_METHOD_DESCRIPTORS[argumentCount]);
		} else {
			stackPushNull();
			mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, SEND_METHOD_NAME, SEND_METHOD_DESCRIPTORS[argumentCount]);
		}
	}

	public void callPrimitiveSymbol(String value, int line) {
		visitLine(line);
		stackPushReceiver();
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_SYMBOL_METHOD_NAME, PRIMITIVE_SYMBOL_SIGNATURE);
	}
}
