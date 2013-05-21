/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;

public class BlockBytecodeWriter extends ClassBytecodeWriter implements Opcodes {

    private static final String INVOKE_SIG = "(Lst/redline/lang/ProtoObject;Lst/redline/lang/PrimContext;)Lst/redline/lang/ProtoObject;";

    public BlockBytecodeWriter(ClassVisitor classVisitor, ClassWriter classWriter, String className, String packageName) {
        super(classVisitor, classWriter, className, packageName);
    }

    protected String superclass() {
        return "st/redline/lang/ProtoBlock";
    }

    protected void openInitializeMethod() {
        createInitializeMethodWithContext();
        super.openInitializeMethod();
    }

    private void createInitializeMethodWithContext() {
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/lang/PrimContext;)V", null, null);
        mv.visitCode();
        visitLine(0);
        mv.visitVarInsn(ALOAD, 0);
        mv.visitVarInsn(ALOAD, 1);
        mv.visitMethodInsn(INVOKESPECIAL, superclass(), "<init>", "(Lst/redline/lang/PrimContext;)V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 3);
        mv.visitEnd();
    }

    protected void invokeInitializeEigenClass() {
    }

    protected void invokeUnInitializeEigenClass() {
    }

    protected void addClassToImports() {
    }

    protected void deregisterPackage() {
    }

    protected void registerPackage() {
    }

    protected void invokeMessageSends() {
    }

    protected void openMessageSendsMethod() {
        openInvokeMethod();
    }

    protected void openInvokeMethod() {
        mv = cv.visitMethod(ACC_PROTECTED, "invoke", INVOKE_SIG, null, null);
        mv.visitCode();
        pushReceiver();
    }
}
