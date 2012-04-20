package st.redline.compiler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import st.redline.ClassPathUtilities;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

public class ClassBytecodeWriter implements Opcodes {

	private static final String OBJECT = "st/redline/PrimObject";
	private static final String CONTEXT = "st/redline/PrimContext";
	private static final String SEND_MESSAGES = "_sendMessages_";
	private static final String SEND_MESSAGES_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";
	private static final String PRIMITIVE_SIG = "(Lst/redline/PrimObject;Lst/redline/PrimContext;)Lst/redline/PrimObject;";
	private static final String[] SIGNATURES = {
		"(Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;",
		"(Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Lst/redline/PrimObject;Ljava/lang/String;)Lst/redline/PrimObject;"
	};

	private final static Map<String, Integer> OPCODES = new HashMap<String, Integer>();

	private final String className;
	private final String packageName;
	private final boolean verbose;

	protected ClassWriter cw;
	protected MethodVisitor mv;
	private String fullyQualifiedClassName;

	ClassBytecodeWriter(String className, String packageName, boolean verbose) {
		this(className, packageName, verbose, null);
		this.cw = createClassWriter();
	}

	ClassBytecodeWriter(String className, String packageName, boolean verbose, ClassWriter classWriter) {
		this.className = className;
		this.packageName = packageName;
		this.verbose = verbose;
		this.cw = classWriter;
		fullyQualifiedClassName = ClassPathUtilities.classNameToFullyQualifiedClassName(packageName, className);
	}

	ClassWriter createClassWriter() {
		return verbose ? tracingClassWriter() : nonTracingClassWriter();
	}

	static ClassWriter nonTracingClassWriter() {
		return new ClassWriter(ClassWriter.COMPUTE_FRAMES);
	}

	static ClassWriter tracingClassWriter() {
		return new TracingClassWriter(ClassWriter.COMPUTE_FRAMES, new PrintWriter(System.out));
	}

	ClassWriter classWriter() {
		return cw;
	}

	byte[] contents() {
		return cw.toByteArray();
	}

	String homogenize(String className) {
		int index = className.indexOf("$");
		if (index == -1)
			return className;
		return className.substring(0, index);
	}

	void methodVisitor(MethodVisitor methodVisitor) {
		mv = methodVisitor;
	}

	void openClass() {
		cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fullyQualifiedClassName, null, superclass(), null);
		cw.visitSource(homogenize(fullyQualifiedClassName) + ".st", null);
		writeInitializeMethod();
		openMessageSendsMethod();
	}

	void openMessageSendsMethod() {
		mv = cw.visitMethod(ACC_PROTECTED, SEND_MESSAGES, SEND_MESSAGES_SIG, null, null);
		mv.visitCode();
		pushThis();
		pushReceiver();
		pushContext();
		mv.visitMethodInsn(INVOKESPECIAL, superclass(), SEND_MESSAGES, SEND_MESSAGES_SIG);
		pop();
		pushReceiver();
	}

	void writeInitializeMethod() {
		openInitializeMethod();
		registerPackage();
		addClassToImports();
		invokeMessageSends();
		deregisterPackage();
		closeInitializeMethod();
	}

	void addClassToImports() {
		pushThis();
		pushLiteral(className);
		pushLiteral(ClassPathUtilities.fullyQualifiedClassNameToPackageName(fullyQualifiedClassName));
		mv.visitMethodInsn(INVOKEVIRTUAL, superclass(), "packageAtPut", "(Ljava/lang/String;Ljava/lang/String;)V");
	}

	void deregisterPackage() {
		if ("".equals(packageName))
			return;
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "deregisterPackage", "()V");
	}

	void registerPackage() {
		if ("".equals(packageName))
			return;
		pushLiteral(packageName);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "registerPackage", "(Ljava/lang/String;)V");
	}

	void invokeMessageSends() {
		pushThis();
		pushThis();
		mv.visitTypeInsn(NEW, "st/redline/PrimContext");
		pushDuplicate();
		pushThis();
		mv.visitMethodInsn(INVOKESPECIAL, "st/redline/PrimContext", "<init>", "(Lst/redline/PrimObject;)V");
		mv.visitMethodInsn(INVOKEVIRTUAL, fullyQualifiedClassName, SEND_MESSAGES, SEND_MESSAGES_SIG);
	}

	String superclass() {
		return "st/redline/PrimObjectMetaclass";
	}

	void closeClass() {
		closeMessageSendsMethod();
		cw.visitEnd();
	}

	void closeMessageSendsMethod() {
		mv.visitInsn(ARETURN);
		closeMethod();
	}

	void closeInitializeMethod() {
		mv.visitInsn(RETURN);
		closeMethod();
	}

	private void closeMethod() {
		mv.visitMaxs(1, 3);
		mv.visitEnd();
	}

	void openInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
		mv.visitCode();
		visitLine(0);
		pushThis();
		mv.visitMethodInsn(INVOKESPECIAL, superclass(), "<init>", "()V");
	}

	void invokeContextTemporariesInit(int size) {
		pushContext();
		pushNumber(size);
		mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporariesInit", "(I)V");
	}

	void invokeObjectPerform(String selector, int argumentCount, boolean sendToSuper) {
		pushLiteral(selector);
		if (sendToSuper)
			mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "superPerform", "(Lst/redline/PrimContext;" + SIGNATURES[argumentCount].substring(1));
		else
			mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "perform", SIGNATURES[argumentCount]);
	}

	void invokeObjectCreate(String type, String value, int line) {
		visitLine(line);
		pushLiteral(value);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, type, "(Ljava/lang/Object;)Lst/redline/PrimObject;");
	}

	void invokeObjectCompileBlock(String name, int line) {
		visitLine(line);
		pushReceiver();
		pushLiteral(name);
		mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "block", "(Ljava/lang/String;)Lst/redline/PrimObject;");
	}

	void invokeObjectString(String value, int line) {
		invokeObjectCreate("string", value, line);
	}

	void invokeObjectSymbol(String value, int line) {
		invokeObjectCreate("symbol", value, line);
	}

	void invokeObjectCharacter(String value, int line) {
		invokeObjectCreate("character", value, line);
	}

	void invokeObjectNumber(String value, int line) {
		invokeObjectCreate("number", value, line);
	}

	void invokeVariableAt(String name, int line) {
		visitLine(line);
		pushReceiver();
		pushLiteral(name);
		mv.visitMethodInsn(INVOKEVIRTUAL, OBJECT, "variableAt", "(Ljava/lang/String;)Lst/redline/PrimObject;");
	}

	void invokeVariablePutAt(String name, int line) {
		visitLine(line);
		pushLiteral(name);
		pushReceiver();
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "variablePutAtIn", "(Lst/redline/PrimObject;Ljava/lang/String;Lst/redline/PrimObject;)Lst/redline/PrimObject;");
	}

	void invokeObjectArray(int size) {
		pushNumber(size);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "array", "(I)Lst/redline/PrimObject;");
	}

	void invokeArrayPutAt(int index, int line) {
		visitLine(line);
		pushNumber(index);
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "putAt", "(Lst/redline/PrimObject;Lst/redline/PrimObject;I)Lst/redline/PrimObject;");
	}

	void invokePrimitive(int line, String primitive) {
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
		mv.visitMethodInsn(INVOKESTATIC, OBJECT, "blockAnswer", "(Lst/redline/PrimObject;Lst/redline/PrimObjectBlock;Ljava/lang/String;)Lst/redline/PrimObject;");
	}

	void pop() {
		mv.visitInsn(POP);
	}

	void pushArgument(int index) {
		pushContext();
		pushNumber(index);
		mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "argumentAt", "(I)Lst/redline/PrimObject;");
	}

	void pushTemporary(int index) {
		pushContext();
		pushNumber(index);
		mv.visitMethodInsn(INVOKEVIRTUAL, CONTEXT, "temporaryAt", "(I)Lst/redline/PrimObject;");
	}

	void storeTemporary(int index) {
		pushNumber(index);
		pushContext();
		mv.visitMethodInsn(INVOKESTATIC, CONTEXT, "temporaryPutAtIn", "(Lst/redline/PrimObject;ILst/redline/PrimContext;)V");
	}

	void pushLiteral(String literal) {
		mv.visitLdcInsn(literal);
	}

	void pushDuplicate() {
		mv.visitInsn(DUP);
	}

	void pushThis() {
		mv.visitVarInsn(ALOAD, 0);
	}

	void pushReceiver() {
		mv.visitVarInsn(ALOAD, 1);
	}

	void pushContext() {
		mv.visitVarInsn(ALOAD, 2);
	}

	void pushNull() {
		mv.visitInsn(ACONST_NULL);
	}

	void pushObjectStaticField(String field) {
		mv.visitFieldInsn(GETSTATIC, OBJECT, field, "Lst/redline/PrimObject;");
	}

	void pushNumber(int value) {
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

	void visitLine(int line) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(line, l0);
	}

    void setupTryForBlockReturn(SimpleExpression simpleExpression, String blockReturnType) {
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

    void setupCatchForBlockReturn(SimpleExpression simpleExpression, String blockReturnType) {
        Label l1 = (Label) simpleExpression.label1();
        Label l2 = (Label) simpleExpression.label2();

        mv.visitLabel(l1);
        Label l3 = new Label();
        mv.visitJumpInsn(GOTO, l3);
        mv.visitLabel(l2);

        mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, new Object[] {blockReturnType});
        mv.visitMethodInsn(INVOKEVIRTUAL, blockReturnType, "answer", "()Lst/redline/PrimObject;");
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

    void visitFieldInsn(int opcode, String owner, String name, String desc) {
        mv.visitFieldInsn(opcode, owner, name, desc);
    }

	void visitMethodInsn(int opcode, String owner, String name, String desc) {
		mv.visitMethodInsn(opcode, owner, name, desc);
	}

	void visitLdcInsn(String value) {
		mv.visitLdcInsn(value);
	}

	void visitVarInsn(int opcode, int value) {
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
