package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.File;
import java.io.PrintWriter;

public class ClassBytecodeWriter implements Opcodes {

	private static final String PROTOOBJECT = "st/redline/ProtoObject";
	private static final String SUPERCLASS = PROTOOBJECT;
	private static final String SEND = "primitiveSend";
	private static final String SUPER_SEND = "primitiveSuperSend";
	private static final String CONSTRUCT = "construct";
	private static final String CONSTRUCT_SIGNATURE = "(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;";
	private static final String PRIMITIVE_SYMBOL = "primitiveSymbol";
	private static final String PRIMITIVE_SYMBOL_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String INIT = "<init>";
	private static final String INIT_SIGNATURE = "()V";
	private static final String PRIMITIVE_VARIABLE_AT = "primitiveVariableAt";
	private static final String PRIMITIVE_VARIABLE_AT_SIGNATURE = "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;";
	private static final String[] SEND_SIGNATURES = {
		"(Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
	};

	private final String className;
	private String packageName;
	private ClassWriter cw;
	private MethodVisitor mv;
	private String fullyQualifiedClassName;
	private int currentLine = 0;

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
		cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, SUPERCLASS, null);
		cw.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
		writeInitializeMethod();
		openApplyToMethod();
	}

	private void openApplyToMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, CONSTRUCT, CONSTRUCT_SIGNATURE, null, null);
		mv.visitCode();
	}

	private void writeInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, INIT, INIT_SIGNATURE, null, null);
		mv.visitCode();
		stackPushThis();
		mv.visitMethodInsn(INVOKESPECIAL, SUPERCLASS, INIT, INIT_SIGNATURE);
		stackPushLiteral(packageName);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitivePackageRegistryCurrent", "(Ljava/lang/String;)V");
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitLdcInsn("ProtoObject");
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitiveResolveObject", "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitInsn(ACONST_NULL);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitiveCreateSubclass", "(Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitInsn(ACONST_NULL);
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, CONSTRUCT, CONSTRUCT_SIGNATURE);
		mv.visitInsn(POP);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitivePackageRegistryRemove", "()V");
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
		// Note: we don't visit the same line repeatedly. Is this and ok debug experience? Need to check with java debugger.
		if (line == currentLine)
			return;
		currentLine = line;
		Label label = new Label();
		mv.visitLabel(label);
		mv.visitLineNumber(line, label);
	}

	public void callPrimitiveVariableAt(String value, int line, boolean loadSide) {
		visitLine(line);
		stackPushReceiver();
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
			mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, SUPER_SEND, SEND_SIGNATURES[argumentCount]);
		} else {
			stackPushNull();
			mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, SEND, SEND_SIGNATURES[argumentCount]);
		}
	}

	public void callPrimitiveSymbol(String value, int line) {
		visitLine(line);
		stackPushReceiver();
		stackPushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, PRIMITIVE_SYMBOL, PRIMITIVE_SYMBOL_SIGNATURE);
	}
}
