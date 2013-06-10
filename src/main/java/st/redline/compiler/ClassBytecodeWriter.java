/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.objectweb.asm.*;
import st.redline.compiler.ast.SimpleExpression;
import st.redline.compiler.ast.Number;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

public class ClassBytecodeWriter implements Opcodes {

    private static final String OBJECT = "st/redline/lang/ProtoObject";
    private static final String CONTEXT = "st/redline/lang/PrimContext";
    private static final String SEND_MESSAGES = "_sendMessages_";
    private static final String SEND_MESSAGES_SIG = "(Lst/redline/lang/ProtoObject;Lst/redline/lang/PrimContext;)Lst/redline/lang/ProtoObject;";
    private static final String RESULT = "sendMessagesResult";
    private static final String RESULT_SIG = "(Lst/redline/lang/ProtoObject;)V";
    private static final String EIGEN_INIT = "initialize";
    private static final String EIGEN_INIT_SIG = "(Ljava/lang/String;Ljava/lang/String;)V";
    private static final String EIGEN_UNINIT = "uninitialize";
    private static final String EIGEN_UNINIT_SIG = "()V";
    private static final String PRIMITIVE_SIG = "(Lst/redline/lang/ProtoObject;Lst/redline/lang/PrimContext;)Lst/redline/lang/ProtoObject;";
    private static final String[] SIGNATURES = {
            "(Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;",
            "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;"
    };
    private static final Map<String, Integer> OPCODES = new HashMap<String, Integer>();
    public final static int BYTECODE_VERSION;
    static {
        int compareTo17 = new BigDecimal(System.getProperty("java.specification.version")).compareTo(new BigDecimal("1.7"));
        if (compareTo17 >= 0) {
            BYTECODE_VERSION = V1_7;
        } else {
            BYTECODE_VERSION = V1_6;
        }
    }

    private final String className;
    private final String packageName;
    private final String fullyQualifiedClassName;

    private final ClassWriter cw;
    protected final ClassVisitor cv;
    protected MethodVisitor mv;

    public ClassBytecodeWriter(ClassVisitor classVisitor, ClassWriter classWriter, String className, String packageName) {
        this.cv = classVisitor;
        this.cw = classWriter;
        this.className = className;
        this.packageName = packageName;
        this.fullyQualifiedClassName = packageName + "/" + className;
    }

    public byte[] contents() {
        return cw.toByteArray();
    }

    public void openClass() {
        cv.visit(BYTECODE_VERSION, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, superclass(), null);
        cv.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
        writeInitializeMethod();
        openMessageSendsMethod();
    }

    public void closeClass() {
        closeMessageSendsMethod();
        cv.visitEnd();
    }

    private void closeMessageSendsMethod() {
        mv.visitInsn(ARETURN);
        closeMethod();
    }

    protected void closeInitializeMethod() {
        mv.visitInsn(RETURN);
        closeMethod();
    }

    private void closeMethod() {
        mv.visitMaxs(1, 3);
        mv.visitEnd();
    }

    protected void openMessageSendsMethod() {
        mv = cv.visitMethod(ACC_PROTECTED, SEND_MESSAGES, SEND_MESSAGES_SIG, null, null);
        mv.visitCode();
        pushThis();
        pushReceiver();
        pushContext();
        mv.visitMethodInsn(INVOKESPECIAL, superclass(), SEND_MESSAGES, SEND_MESSAGES_SIG);
        pop();
        pushReceiver();
    }

    protected void writeInitializeMethod() {
        openInitializeMethod();
        invokeInitializeEigenClass();
        invokeMessageSends();
        invokeUnInitializeEigenClass();
        closeInitializeMethod();
    }

    protected void openInitializeMethod() {
        mv = cv.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        visitLine(0);
        pushThis();
        mv.visitMethodInsn(INVOKESPECIAL, superclass(), "<init>", "()V");
    }

    protected void invokeInitializeEigenClass() {
        pushThis();
        pushLiteral(className);
        pushLiteral(packageName);
        mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, EIGEN_INIT, EIGEN_INIT_SIG);
    }

    protected void invokeUnInitializeEigenClass() {
        pushThis();
        mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, EIGEN_UNINIT, EIGEN_UNINIT_SIG);
    }

    protected void invokeMessageSends() {
        pushThis();
        pushThis();
        pushThis();
        mv.visitTypeInsn(NEW, "st/redline/lang/PrimContext");
        pushDuplicate();
        pushThis();
        mv.visitMethodInsn(INVOKESPECIAL, "st/redline/lang/PrimContext", "<init>", "(Lst/redline/lang/ProtoObject;)V");
        mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, SEND_MESSAGES, SEND_MESSAGES_SIG);
        mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, RESULT, RESULT_SIG);
    }

    private String homogenize(String className) {
        int index = className.indexOf("$");
        if (index == -1)
            return className;
        return className.substring(0, index);
    }

    protected String superclass() {
        return "st/redline/lang/EigenClass";
    }

    public void addClassToImports(String importClassName) {
        pushReceiver();
        pushDuplicate();
        visitLdcInsn(importClassName);
        visitMethodInsn(INVOKEVIRTUAL, "st/redline/lang/ProtoObject", "smalltalkString", "(Ljava/lang/Object;)Lst/redline/lang/ProtoObject;");
        visitLdcInsn("import:");
        visitMethodInsn(INVOKEVIRTUAL, "st/redline/lang/ProtoObject", "perform", "(Lst/redline/lang/ProtoObject;Ljava/lang/String;)Lst/redline/lang/ProtoObject;");
        pop();
    }

    public void invokeContextTemporariesInit(int size) {
        pushContext();
        pushNumber(size);
        mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporariesInit", "(I)V");
    }

    public void invokeObjectPerform(String selector, int argumentCount, boolean sendToSuper) {
        pushLiteral(selector);
        if (sendToSuper)
            mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "superPerform", "(Lst/redline/lang/PrimContext;" + SIGNATURES[argumentCount].substring(1));
        else
            mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "perform", SIGNATURES[argumentCount]);
    }

    public void invokeObjectCreate(String type, String value, int line) {
        visitLine(line);
        pushReceiver();
        pushLiteral(value);
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, type, "(Ljava/lang/Object;)Lst/redline/lang/ProtoObject;");
    }

    public void invokeObjectCompileBlock(String name, int line) {
        visitLine(line);
        pushReceiver();
        pushLiteral(name);
        pushContext();
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "block", "(Ljava/lang/String;Lst/redline/lang/PrimContext;)Lst/redline/lang/ProtoObject;");
    }

    public void invokeObjectString(String value, int line) {
        invokeObjectCreate("smalltalkString", value, line);
    }

    public void invokeObjectSymbol(String value, int line) {
        invokeObjectCreate("smalltalkSymbol", value, line);
    }

    public void invokeObjectCharacter(String value, int line) {
        invokeObjectCreate("smalltalkCharacter", value, line);
    }

    public void invokeObjectNumber(String value, int line) {
        invokeObjectCreate("smalltalkNumber", value, line);
    }

    public void invokeVariableAt(String name, int line) {
        visitLine(line);
        pushReceiver();
        pushLiteral(name);
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "variableAt", "(Ljava/lang/String;)Lst/redline/lang/ProtoObject;");
    }

    public void invokeVariablePutAt(String name, int line) {
        visitLine(line);
        pushLiteral(name);
        pushReceiver();
        mv.visitMethodInsn(INVOKESTATIC, OBJECT, "variablePutAtIn", "(Lst/redline/lang/ProtoObject;Ljava/lang/String;Lst/redline/lang/ProtoObject;)Lst/redline/lang/ProtoObject;");
    }

    public void invokeObjectArray(int size) {
        pushReceiver();
        pushNumber(size);
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "smalltalkArray", "(I)Lst/redline/lang/ProtoObject;");
    }

    public void invokeArrayPutAt(int index, int line) {
        visitLine(line);
        pushNumber(index);
        mv.visitMethodInsn(INVOKESTATIC, OBJECT, "putAt", "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoObject;I)Lst/redline/lang/ProtoObject;");
    }

    public void invokePrimitive(int line, String primitive) {
        // TODO.JCL - cater for case where primitive fails - for now return primitive result.
        // Doing ARETURN here means there can be more than one ARETURN emitted, this is OK.
        visitLine(line);
        pushThis();
        pushReceiver();
        pushContext();
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "p" + primitive, PRIMITIVE_SIG);
        mv.visitInsn(ARETURN);
    }

    public void invokeBlockAnswer(String blockReturnType) {
        pushThis();
        pushLiteral(blockReturnType);
        mv.visitMethodInsn(INVOKESTATIC, OBJECT, "blockAnswer", "(Lst/redline/lang/ProtoObject;Lst/redline/lang/ProtoBlock;Ljava/lang/String;)Lst/redline/lang/ProtoObject;");
    }

    public void pop() {
        mv.visitInsn(POP);
    }

    public void pushArgument(int index) {
        pushContext();
        pushNumber(index);
        mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "argumentAt", "(I)Lst/redline/lang/ProtoObject;");
    }

    public void pushArgumentElement(int argumentIndex, int elementIndex) {
        pushContext();
        pushNumber(argumentIndex);
        pushNumber(elementIndex);
        mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "argumentAtAt", "(II)Lst/redline/lang/ProtoObject;");
    }

    public void pushOuterArgument(int index) {
        pushContext();
        pushNumber(index);
        pushReceiver();
        mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "argumentAtFrom", "(ILst/redline/lang/ProtoObject;)Lst/redline/lang/ProtoObject;");
    }

    public void pushTemporary(int index) {
        pushContext();
        pushNumber(index);
        mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporaryAt", "(I)Lst/redline/lang/ProtoObject;");
    }

    public void pushOuterTemporary(int index) {
        pushContext();
        pushNumber(index);
        pushReceiver();
        mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporaryAtFrom", "(ILst/redline/lang/ProtoObject;)Lst/redline/lang/ProtoObject;");
    }

    public void storeTemporary(int index) {
        pushNumber(index);
        pushContext();
        mv.visitMethodInsn(INVOKESTATIC, CONTEXT, "temporaryPutAtIn", "(Lst/redline/lang/ProtoObject;ILst/redline/lang/PrimContext;)V");
    }

    public void storeOuterTemporary(int index) {
        pushNumber(index);
        pushContext();
        pushReceiver();
        mv.visitMethodInsn(INVOKESTATIC, CONTEXT, "temporaryPutAtInFrom", "(Lst/redline/lang/ProtoObject;ILst/redline/lang/PrimContext;Lst/redline/lang/ProtoObject;)V");
    }

    public void pushOuterReceiver() {
        pushReceiver();
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "outerReceiver", "()Lst/redline/lang/ProtoObject;");
    }

    public void pushLiteral(String literal) {
        mv.visitLdcInsn(literal);
    }

    public void pushDuplicate() {
        mv.visitInsn(DUP);
    }

    public void pushThis() {
        mv.visitVarInsn(ALOAD, 0);
    }

    public void pushReceiver() {
        mv.visitVarInsn(ALOAD, 1);
    }

    public void pushContext() {
        mv.visitVarInsn(ALOAD, 2);
    }

    public void pushNull() {
        mv.visitInsn(ACONST_NULL);
    }

    public void pushObjectStaticField(String field) {
        mv.visitFieldInsn(GETSTATIC, OBJECT, field, "Lst/redline/lang/ProtoObject;");
    }

    public void pushObjectField(String object, String field) {
        mv.visitFieldInsn(GETFIELD, object, field, "Lst/redline/lang/ProtoObject;");
    }

    public void pushObjectStaticField(String object, String field) {
        mv.visitFieldInsn(GETSTATIC, object, field, "Lst/redline/lang/ProtoObject;");
    }

    public void pushClassLoaderField(String field) {
        pushClassLoader();
        pushObjectField("st/redline/classloader/SmalltalkClassLoader", field);
    }

    public void pushClassLoader() {
        pushReceiver();
        mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "classLoader", "()Lst/redline/classloader/SmalltalkClassLoader;");
    }

    public void pushNumber(int value) {
        switch (value) {
            case 0: mv.visitInsn(ICONST_0); break;
            case 1: mv.visitInsn(ICONST_1); break;
            case 2: mv.visitInsn(ICONST_2); break;
            case 3: mv.visitInsn(ICONST_3); break;
            case 4: mv.visitInsn(ICONST_4); break;
            case 5: mv.visitInsn(ICONST_5); break;
            default:
                if (value > 5 && value < 128)
                    mv.visitIntInsn(BIPUSH, value);
                else // SIPUSH not supported yet.
                    throw new IllegalStateException("push of integer value " + value + " not yet supported.");
        }
    }

    public void visitLine(int line) {
        Label l0 = new Label();
        mv.visitLabel(l0);
        mv.visitLineNumber(line, l0);
    }

    public void setupTryForBlockReturn(SimpleExpression simpleExpression, String blockReturnType) {
//        System.out.println("setupTryForBlockReturn: " + simpleExpression);
        Label l0 = new Label();
        Label l1 = new Label();
        Label l2 = new Label();

        simpleExpression.leaveResultOnStack();
        simpleExpression.label0(l0);
        simpleExpression.label1(l1);
        simpleExpression.label2(l2);

        mv.visitTryCatchBlock(l0, l1, l2, blockReturnType);
        mv.visitLabel(l0);
    }

    public void setupCatchForBlockReturn(SimpleExpression simpleExpression, String blockReturnType) {
//        System.out.println("setupCatchForBlockReturn: " + simpleExpression);
        Label l1 = (Label) simpleExpression.label1();
        Label l2 = (Label) simpleExpression.label2();

        mv.visitLabel(l1);
        Label l3 = new Label();
        mv.visitJumpInsn(GOTO, l3);
        mv.visitLabel(l2);

        mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, new Object[] {blockReturnType});
        mv.visitMethodInsn(INVOKEVIRTUAL, blockReturnType, "answer", "()Lst/redline/lang/ProtoObject;");
        mv.visitInsn(ARETURN);

        mv.visitLabel(l3);
        mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
    }

    public void visitInsn(String opcode) {
        mv.visitInsn(opcodeValue(opcode));
    }

    private int opcodeValue(String opcode) {
        if (!OPCODES.containsKey(opcode))
            throw new IllegalStateException("Unknown OPCODE '" + opcode + "'.");
        return OPCODES.get(opcode);
    }

    public void visitFieldInsn(int opcode, String owner, String name, String desc) {
        mv.visitFieldInsn(opcode, owner, name, desc);
    }

    public void visitMethodInsn(int opcode, String owner, String name, String desc) {
        mv.visitMethodInsn(opcode, owner, name, desc);
    }

    public void visitLdcInsn(String value) {
        mv.visitLdcInsn(value);
    }

    public void visitVarInsn(int opcode, int value) {
        mv.visitVarInsn(opcode, value);
    }

    public void visitTypeInsn(int opcode, String type) {
        mv.visitTypeInsn(opcode, type);
    }

    static {
        OPCODES.put("V1_1", 196653);
        OPCODES.put("V1_2", 46);
        OPCODES.put("V1_3", 47);
        OPCODES.put("V1_4", 48);
        OPCODES.put("V1_5", 49);
        OPCODES.put("V1_6", 50);
        OPCODES.put("V1_7", 51);
        OPCODES.put("ACC_PUBLIC", 1);
        OPCODES.put("ACC_PRIVATE", 2);
        OPCODES.put("ACC_PROTECTED", 4);
        OPCODES.put("ACC_STATIC", 8);
        OPCODES.put("ACC_FINAL", 16);
        OPCODES.put("ACC_SUPER", 32);
        OPCODES.put("ACC_SYNCHRONIZED", 32);
        OPCODES.put("ACC_VOLATILE", 64);
        OPCODES.put("ACC_BRIDGE", 64);
        OPCODES.put("ACC_VARARGS", 128);
        OPCODES.put("ACC_TRANSIENT", 128);
        OPCODES.put("ACC_NATIVE", 256);
        OPCODES.put("ACC_INTERFACE", 512);
        OPCODES.put("ACC_ABSTRACT", 1024);
        OPCODES.put("ACC_STRICT", 2048);
        OPCODES.put("ACC_SYNTHETIC", 4096);
        OPCODES.put("ACC_ANNOTATION", 8192);
        OPCODES.put("ACC_ENUM", 16384);
        OPCODES.put("ACC_DEPRECATED", 131072);
        OPCODES.put("T_BOOLEAN", 4);
        OPCODES.put("T_CHAR", 5);
        OPCODES.put("T_FLOAT", 6);
        OPCODES.put("T_DOUBLE", 7);
        OPCODES.put("T_BYTE", 8);
        OPCODES.put("T_SHORT", 9);
        OPCODES.put("T_INT", 10);
        OPCODES.put("T_LONG", 11);
        OPCODES.put("F_NEW", -1);
        OPCODES.put("F_FULL", 0);
        OPCODES.put("F_APPEND", 1);
        OPCODES.put("F_CHOP", 2);
        OPCODES.put("F_SAME", 3);
        OPCODES.put("F_SAME1", 4);
        OPCODES.put("TOP", TOP);
        OPCODES.put("INTEGER", INTEGER);
        OPCODES.put("FLOAT", FLOAT);
        OPCODES.put("DOUBLE", DOUBLE);
        OPCODES.put("LONG", LONG);
        OPCODES.put("NULL", NULL);
        OPCODES.put("UNINITIALIZED_THIS", UNINITIALIZED_THIS);
        OPCODES.put("NOP", 0);
        OPCODES.put("ACONST_NULL", 1);
        OPCODES.put("ICONST_M1", 2);
        OPCODES.put("ICONST_0", 3);
        OPCODES.put("ICONST_1", 4);
        OPCODES.put("ICONST_2", 5);
        OPCODES.put("ICONST_3", 6);
        OPCODES.put("ICONST_4", 7);
        OPCODES.put("ICONST_5", 8);
        OPCODES.put("LCONST_0", 9);
        OPCODES.put("LCONST_1", 10);
        OPCODES.put("FCONST_0", 11);
        OPCODES.put("FCONST_1", 12);
        OPCODES.put("FCONST_2", 13);
        OPCODES.put("DCONST_0", 14);
        OPCODES.put("DCONST_1", 15);
        OPCODES.put("BIPUSH", 16);
        OPCODES.put("SIPUSH", 17);
        OPCODES.put("LDC", 18);
        OPCODES.put("ILOAD", 21);
        OPCODES.put("LLOAD", 22);
        OPCODES.put("FLOAD", 23);
        OPCODES.put("DLOAD", 24);
        OPCODES.put("ALOAD", 25);
        OPCODES.put("IALOAD", 46);
        OPCODES.put("LALOAD", 47);
        OPCODES.put("FALOAD", 48);
        OPCODES.put("DALOAD", 49);
        OPCODES.put("AALOAD", 50);
        OPCODES.put("BALOAD", 51);
        OPCODES.put("CALOAD", 52);
        OPCODES.put("SALOAD", 53);
        OPCODES.put("ISTORE", 54);
        OPCODES.put("LSTORE", 55);
        OPCODES.put("FSTORE", 56);
        OPCODES.put("DSTORE", 57);
        OPCODES.put("ASTORE", 58);
        OPCODES.put("IASTORE", 79);
        OPCODES.put("LASTORE", 80);
        OPCODES.put("FASTORE", 81);
        OPCODES.put("DASTORE", 82);
        OPCODES.put("AASTORE", 83);
        OPCODES.put("BASTORE", 84);
        OPCODES.put("CASTORE", 85);
        OPCODES.put("SASTORE", 86);
        OPCODES.put("POP", 87);
        OPCODES.put("POP2", 88);
        OPCODES.put("DUP", 89);
        OPCODES.put("DUP_X1", 90);
        OPCODES.put("DUP_X2", 91);
        OPCODES.put("DUP2", 92);
        OPCODES.put("DUP2_X1", 93);
        OPCODES.put("DUP2_X2", 94);
        OPCODES.put("SWAP", 95);
        OPCODES.put("IADD", 96);
        OPCODES.put("LADD", 97);
        OPCODES.put("FADD", 98);
        OPCODES.put("DADD", 99);
        OPCODES.put("ISUB", 100);
        OPCODES.put("LSUB", 101);
        OPCODES.put("FSUB", 102);
        OPCODES.put("DSUB", 103);
        OPCODES.put("IMUL", 104);
        OPCODES.put("LMUL", 105);
        OPCODES.put("FMUL", 106);
        OPCODES.put("DMUL", 107);
        OPCODES.put("IDIV", 108);
        OPCODES.put("LDIV", 109);
        OPCODES.put("FDIV", 110);
        OPCODES.put("DDIV", 111);
        OPCODES.put("IREM", 112);
        OPCODES.put("LREM", 113);
        OPCODES.put("FREM", 114);
        OPCODES.put("DREM", 115);
        OPCODES.put("INEG", 116);
        OPCODES.put("LNEG", 117);
        OPCODES.put("FNEG", 118);
        OPCODES.put("DNEG", 119);
        OPCODES.put("ISHL", 120);
        OPCODES.put("LSHL", 121);
        OPCODES.put("ISHR", 122);
        OPCODES.put("LSHR", 123);
        OPCODES.put("IUSHR", 124);
        OPCODES.put("LUSHR", 125);
        OPCODES.put("IAND", 126);
        OPCODES.put("LAND", 127);
        OPCODES.put("IOR", 128);
        OPCODES.put("LOR", 129);
        OPCODES.put("IXOR", 130);
        OPCODES.put("LXOR", 131);
        OPCODES.put("IINC", 132);
        OPCODES.put("I2L", 133);
        OPCODES.put("I2F", 134);
        OPCODES.put("I2D", 135);
        OPCODES.put("L2I", 136);
        OPCODES.put("L2F", 137);
        OPCODES.put("L2D", 138);
        OPCODES.put("F2I", 139);
        OPCODES.put("F2L", 140);
        OPCODES.put("F2D", 141);
        OPCODES.put("D2I", 142);
        OPCODES.put("D2L", 143);
        OPCODES.put("D2F", 144);
        OPCODES.put("I2B", 145);
        OPCODES.put("I2C", 146);
        OPCODES.put("I2S", 147);
        OPCODES.put("LCMP", 148);
        OPCODES.put("FCMPL", 149);
        OPCODES.put("FCMPG", 150);
        OPCODES.put("DCMPL", 151);
        OPCODES.put("DCMPG", 152);
        OPCODES.put("IFEQ", 153);
        OPCODES.put("IFNE", 154);
        OPCODES.put("IFLT", 155);
        OPCODES.put("IFGE", 156);
        OPCODES.put("IFGT", 157);
        OPCODES.put("IFLE", 158);
        OPCODES.put("IF_ICMPEQ", 159);
        OPCODES.put("IF_ICMPNE", 160);
        OPCODES.put("IF_ICMPLT", 161);
        OPCODES.put("IF_ICMPGE", 162);
        OPCODES.put("IF_ICMPGT", 163);
        OPCODES.put("IF_ICMPLE", 164);
        OPCODES.put("IF_ACMPEQ", 165);
        OPCODES.put("IF_ACMPNE", 166);
        OPCODES.put("GOTO", 167);
        OPCODES.put("JSR", 168);
        OPCODES.put("RET", 169);
        OPCODES.put("TABLESWITCH", 170);
        OPCODES.put("LOOKUPSWITCH", 171);
        OPCODES.put("IRETURN", 172);
        OPCODES.put("LRETURN", 173);
        OPCODES.put("FRETURN", 174);
        OPCODES.put("DRETURN", 175);
        OPCODES.put("ARETURN", 176);
        OPCODES.put("RETURN", 177);
        OPCODES.put("GETSTATIC", 178);
        OPCODES.put("PUTSTATIC", 179);
        OPCODES.put("GETFIELD", 180);
        OPCODES.put("PUTFIELD", 181);
        OPCODES.put("INVOKEVIRTUAL", 182);
        OPCODES.put("INVOKESPECIAL", 183);
        OPCODES.put("INVOKESTATIC", 184);
        OPCODES.put("INVOKEINTERFACE", 185);
        OPCODES.put("INVOKEDYNAMIC", 186);
        OPCODES.put("NEW", 187);
        OPCODES.put("NEWARRAY", 188);
        OPCODES.put("ANEWARRAY", 189);
        OPCODES.put("ARRAYLENGTH", 190);
        OPCODES.put("ATHROW", 191);
        OPCODES.put("CHECKCAST", 192);
        OPCODES.put("INSTANCEOF", 193);
        OPCODES.put("MONITORENTER", 194);
        OPCODES.put("MONITOREXIT", 195);
        OPCODES.put("MULTIANEWARRAY", 197);
        OPCODES.put("IFNULL", 198);
        OPCODES.put("IFNONNULL", 199);
    }
}
