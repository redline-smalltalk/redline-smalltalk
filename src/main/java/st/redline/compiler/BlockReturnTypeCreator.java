/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
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
        this.fullyQualifiedClassName = fullyQualifedClassName;
    }

    void create() {
        if (registry.containsKey(fullyQualifiedClassName))
            return;
        registry.put(fullyQualifiedClassName, fullyQualifiedClassName);
        loadClass(createClass());
    }

    private void loadClass(byte[] aClass) {
        try {
            PrimObject.smalltalkClassLoader().defineClass(aClass);
        } catch (Exception e) {
            throw new RedlineException(e);
        }
    }

    private byte[] createClass() {
        ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        writer.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, "st/redline/BlockReturn", null);
        createAnswerArgumentConstructor(writer);
        writer.visitEnd();
        return writer.toByteArray();
    }

    void createAnswerArgumentConstructor(ClassWriter writer) {
        MethodVisitor mv = writer.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/PrimObject;)V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitVarInsn(ALOAD, 1);
        mv.visitMethodInsn(INVOKESPECIAL, "st/redline/BlockReturn", "<init>", "(Lst/redline/PrimObject;)V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(2, 3);
        mv.visitEnd();
    }
}
