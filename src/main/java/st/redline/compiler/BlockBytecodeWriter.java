/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;

public class BlockBytecodeWriter extends ClassBytecodeWriter implements Opcodes {

    private static final String INVOKE_SIG = "(Lst/redline/core/PrimObject;Lst/redline/core/PrimContext;)Lst/redline/core/PrimObject;";

    BlockBytecodeWriter(String className, String packageName, boolean verbose) {
        super(className, packageName, verbose);
    }

    BlockBytecodeWriter(String className, String packageName, boolean verbose, ClassWriter classWriter) {
        super(className, packageName, verbose, classWriter);
    }

    String superclass() {
        return "st/redline/core/PrimObjectBlock";
    }

    void openInitializeMethod() {
        createInitializeMethodWithContext();
        super.openInitializeMethod();
    }

    private void createInitializeMethodWithContext() {
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/core/PrimContext;)V", null, null);
        mv.visitCode();
        visitLine(0);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitVarInsn(ALOAD, 1);
        mv.visitMethodInsn(INVOKESPECIAL, superclass(), "<init>", "(Lst/redline/core/PrimContext;)V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 3);
        mv.visitEnd();
    }

    void addClassToImports() {
    }

    void deregisterPackage() {
    }

    void registerPackage() {
    }

    void invokeMessageSends() {
    }

    void openMessageSendsMethod() {
        openInvokeMethod();
    }

    void openInvokeMethod() {
        mv = cv.visitMethod(ACC_PROTECTED, "invoke", INVOKE_SIG, null, null);
        mv.visitCode();
        pushReceiver();
    }
}
