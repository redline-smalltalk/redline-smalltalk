package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;

public class BlockBytecodeWriter extends ClassBytecodeWriter implements Opcodes {

	private static final String INVOKE_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";

	BlockBytecodeWriter(String className, String packageName, boolean verbose) {
		super(className, packageName, verbose);
	}

	BlockBytecodeWriter(String className, String packageName, boolean verbose, ClassWriter classWriter) {
		super(className, packageName, verbose, classWriter);
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
		pushReceiver();
	}
}
