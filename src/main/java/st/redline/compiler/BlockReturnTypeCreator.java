/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import st.redline.classloader.SmalltalkClassLoader;
import st.redline.compiler.ClassBytecodeWriter;

import java.util.HashMap;
import java.util.Map;

public class BlockReturnTypeCreator implements Opcodes {

    private final String fullyQualifiedClassName;

    public BlockReturnTypeCreator(String fullyQualifedClassName) {
        this.fullyQualifiedClassName = fullyQualifedClassName;
    }

    void create(SmalltalkClassLoader smalltalkClassLoader) {
        if (smalltalkClassLoader.hasBlockReturnType(fullyQualifiedClassName))
            return;
        smalltalkClassLoader.addBlockReturnType(fullyQualifiedClassName);
        loadClass(smalltalkClassLoader, createClass());
    }

    private void loadClass(SmalltalkClassLoader smalltalkClassLoader, byte[] aClass) {
        try {
            smalltalkClassLoader.defineClass(aClass);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private byte[] createClass() {
        ClassWriter writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        writer.visit(ClassBytecodeWriter.BYTECODE_VERSION, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, "st/redline/lang/BlockReturn", null);
        createAnswerArgumentConstructor(writer);
        writer.visitEnd();
        return writer.toByteArray();
    }

    void createAnswerArgumentConstructor(ClassWriter writer) {
        MethodVisitor mv = writer.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/lang/ProtoObject;)V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitVarInsn(ALOAD, 1);
        mv.visitMethodInsn(INVOKESPECIAL, "st/redline/lang/BlockReturn", "<init>", "(Lst/redline/lang/ProtoObject;)V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(2, 3);
        mv.visitEnd();
    }
}
