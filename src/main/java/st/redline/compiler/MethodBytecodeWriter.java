/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class MethodBytecodeWriter extends ClassBytecodeWriter {

	private static final String SUPERCLASS = "st/redline/ProtoMethod";
	private static final String[] APPLY_TO_SIGNATURES = {
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
	};

	private int countOfArguments;

	public MethodBytecodeWriter(String className, String packageName, int countOfArguments) {
		super(className, packageName);
//		System.out.println("MethodBytecodeWriter() " + className + " " + packageName + " " + countOfArguments);
		this.countOfArguments = countOfArguments;
	}

	protected String superclass() {
		return SUPERCLASS;
	}

	protected void openApplyToMethod() {
		if (countOfArguments >= APPLY_TO_SIGNATURES.length)
			throw new IllegalArgumentException(countOfArguments + " arguments to method " + className + " is over argument limit. ");
		mv = cw.visitMethod(ACC_PUBLIC, "applyTo", APPLY_TO_SIGNATURES[countOfArguments], null, null);
		mv.visitCode();

		// TODO.JCL remove - just some debug / trace
//		mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
//		mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
//		mv.visitInsn(DUP);
//		mv.visitLdcInsn("==> applyTo ");
//		mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V");
//		mv.visitVarInsn(ALOAD, 0);
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
//		mv.visitLdcInsn(" ");
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;");
//		mv.visitVarInsn(ALOAD, 1);
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
//		mv.visitLdcInsn(" ");
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;");
//		mv.visitVarInsn(ALOAD, 2);
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
//		mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
	}

	protected void writeInitializeMethod() {
		mv = cw.visitMethod(ACC_PUBLIC, INIT, INIT_SIGNATURE, null, null);
		mv.visitCode();
		stackPushThis();
		mv.visitMethodInsn(INVOKESPECIAL, superclass(), INIT, INIT_SIGNATURE);
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 1);
		mv.visitEnd();
	}

	public void callToPrimitiveByNumber(int methodArgumentCount, int methodTemporariesCount, String primitive, int line) {
		if (methodArgumentCount > APPLY_TO_SIGNATURES.length)
			throw new IllegalStateException("Can't handle " + methodArgumentCount + " arguments to primitive.");
		visitLine(line);
		mv.visitVarInsn(ALOAD, 1);  // receiver.
		mv.visitVarInsn(ALOAD, 2);  // class method was found in.
		pushMethodArguments(methodArgumentCount);
		pushNulls(methodArgumentCount + 2);
		mv.visitMethodInsn(INVOKESTATIC, PROTOOBJECT, "primitive_"+primitive, APPLY_TO_SIGNATURES[APPLY_TO_SIGNATURES.length - 1]);
		// TODO.JCL - cater for case where primitive fails - for now return primitive result.
		// Note: Doing ARETURN here means there can be more than one ARETURN emitted, this is OK.
		mv.visitInsn(ARETURN);
	}

	private void pushMethodArguments(int methodArgumentCount) {
		for (int i = 0; i < methodArgumentCount; i++)
			mv.visitVarInsn(ALOAD, (i + 3));
	}

	private void pushNulls(int countOfValuesPushed) {
		for (int i = countOfValuesPushed; i < APPLY_TO_SIGNATURES.length + 1; i++)
			mv.visitInsn(ACONST_NULL);
	}
}
