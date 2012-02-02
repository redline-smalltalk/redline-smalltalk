package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;

public class BlockBytecodeWriter extends ClassBytecodeWriter implements Opcodes {

	private static final String OBJECT = "st/redline/PrimObject";
	private static final String CONTEXT = "st/redline/PrimContext";
	private static final String INVOKE_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";

	private final int temporariesCount;

	BlockBytecodeWriter(String className, String packageName, boolean verbose, int temporariesCount) {
		super(className, packageName, verbose);
		this.temporariesCount = temporariesCount;
	}

	BlockBytecodeWriter(String className, String packageName, boolean verbose, ClassWriter classWriter, int temporariesCount) {
		super(className, packageName, verbose, classWriter);
		this.temporariesCount = temporariesCount;
	}

	String superclass() {
		return "st/redline/PrimObject";
	}

	void invokeMessageSends() {
	}

	void openMessageSendsMethod() {
		openInvokeMethod();
	}

	void openInvokeMethod() {
		mv = cw.visitMethod(ACC_PROTECTED, "invoke", INVOKE_SIG, null, null);
		mv.visitCode();
		if (temporariesCount > 0) {
			pushContext();
			pushNumber(temporariesCount);
			mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporariesInit", "(I)V");
		}
		pushReceiver();
	}
}
