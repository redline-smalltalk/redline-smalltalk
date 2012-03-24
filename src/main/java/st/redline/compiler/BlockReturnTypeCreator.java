package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import st.redline.ClassPathUtilities;
import st.redline.PrimObject;
import st.redline.RedlineException;

import java.util.HashMap;
import java.util.Map;

public class BlockReturnTypeCreator implements Opcodes {

    private final static Map<String, String> registry = new HashMap<String, String>();

    private final String fullyQualifiedClassName;

    public BlockReturnTypeCreator(String fullyQualifedClassName) {
        this.fullyQualifiedClassName = fullyQualifedClassName.replace(".", "/");
    }

    void create() {
	    System.out.println("blockReturnTypeCreator: " + fullyQualifiedClassName);
        if (registry.containsKey(fullyQualifiedClassName))
            return;
        registry.put(fullyQualifiedClassName, fullyQualifiedClassName);
        loadClass(createClass());
    }

    private void loadClass(byte[] aClass) {
        try {
            Class cls = PrimObject.smalltalkClassLoader().defineClass(aClass);
	        cls.newInstance();
	        System.out.println("BlockReturnTypeCreator class: " + cls);
        } catch (Exception e) {
            throw new RedlineException(e);
        }
    }

	private byte[] createClass() {
		ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		writer.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, "st/redline/BlockReturn", null);
		createAnswerArgumentConstructor(writer);
		writer.visitEnd();
		return writer.toByteArray();
	}

	private void createAnswerArgumentConstructor(ClassWriter writer) {
		MethodVisitor mv = writer.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/PrimObject;)V", null, null);
		mv.visitCode();
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 1);
		mv.visitMethodInsn(INVOKESPECIAL, "st/redline/BlockReturn", "<init>", "(Lst/redline/PrimObject;)V");
		mv.visitInsn(RETURN);
		mv.visitMaxs(2, 2);
		mv.visitEnd();
	}
}
