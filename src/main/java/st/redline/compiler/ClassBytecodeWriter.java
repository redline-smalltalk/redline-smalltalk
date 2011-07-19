package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import java.io.File;
import java.io.PrintWriter;

public class ClassBytecodeWriter implements Opcodes {

	private static final String SUPERCLASS_FULLY_QUALIFIED_NAME = "st/redline/Object";
	private static final String INIT_METHOD = "<init>";
	private static final String INIT_METHOD_SIGNATURE = "()V";

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
		openInitializeMethod();
	}

	private void openInitializeMethod() {
		methodVisitor = classWriter.visitMethod(ACC_PUBLIC, INIT_METHOD, INIT_METHOD_SIGNATURE, null, null);
		methodVisitor.visitCode();
		invokeSuperclassInitMethod();
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
		closeInitializeMethod();
		classWriter.visitEnd();
	}

	private void closeInitializeMethod() {
		methodVisitor.visitInsn(RETURN);
		methodVisitor.visitMaxs(1, 1);
		methodVisitor.visitEnd();
	}

	private void visitLine(int line) {
		Label label = new Label();
		methodVisitor.visitLabel(label);
		methodVisitor.visitLineNumber(line + startingLineNumber, label);
	}

	public void callPrimitiveVariableAt(String value, int line) {
		visitLine(line);
		methodVisitor.visitVarInsn(ALOAD, 0);
		methodVisitor.visitLdcInsn(value);
		methodVisitor.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, "primitiveVariableAt", "(Ljava/lang/String;)Lst/redline/Object;");
	}
}
