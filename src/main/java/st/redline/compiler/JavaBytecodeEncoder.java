package st.redline.compiler;

import org.objectweb.asm.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class JavaBytecodeEncoder extends ClassLoader implements Opcodes {

	public static final String DEFAULT_FILE_PACKAGE = "st/redline/";
	public static final String DEFAULT_JAVA_PACKAGE = "st.redline.";
	private static final String[] METHOD_SIGNATURE = {
		"()Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;"
	};

	private static final String[] SEND_SIGNATURE = {
		"(Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;)Lst/redline/ProtoObject;"
	};

	private static final String[] SUPER_SEND_SIGNATURE = {
		"(Ljava/lang/String;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;",
		"(Ljava/lang/String;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Lst/redline/ProtoObject;Ljava/lang/String;)Lst/redline/ProtoObject;"
	};

	private final ClassWriter classWriter;
	private final ClassWriter classClassWriter;
	private String subclass;
	private String qualifiedSubclass;
	private String classQualifiedSubclass;
	private String qualifiedSuperclass;
	private String classQualifiedSuperclass;
	private String sourcePath;
	private String currentMethodName;
	private Stack<Boolean> superSend = new Stack<Boolean>();
	private List<RawClass> blocksAndBlockAnswers = new ArrayList<RawClass>();
	private Stack<Block> currentBlock = new Stack<Block>();

	public JavaBytecodeEncoder() {
		classWriter = new TracingClassWriter(ClassWriter.COMPUTE_MAXS);
		classClassWriter = new TracingClassWriter(ClassWriter.COMPUTE_MAXS);
	}

	public void defineClass(ClassDefinition classDefinition, ClassInstanceVariables classInstanceVariables, String sourcePath) {
		this.subclass = classDefinition.subclass();
		this.sourcePath = sourcePath;
		qualifiedSubclass = DEFAULT_FILE_PACKAGE + subclass;
		classQualifiedSubclass = DEFAULT_FILE_PACKAGE + subclass + "$mClass";
		qualifiedSuperclass = DEFAULT_FILE_PACKAGE + classDefinition.superclass();
		if (subclass.equals("Object"))
			classQualifiedSuperclass = DEFAULT_FILE_PACKAGE + "Class";
		else
			classQualifiedSuperclass = DEFAULT_FILE_PACKAGE + classDefinition.superclass() + "$mClass";
		String[] classInstanceVariableNames = classInstanceVariables != null ? classInstanceVariables.names() : new String[0];
		defineClass(sourcePath, classDefinition.instanceVariableNames(), classDefinition.classVariableNames(), classInstanceVariableNames);
	}

	private void defineClass(String sourcePath, String[] instanceVariableNames, String[] classVariableNames, String[] classInstanceVariableNames) {
		defineInstanceClass(sourcePath, classWriter, qualifiedSubclass, qualifiedSuperclass);
		defineInstanceFields(instanceVariableNames);
		defineInstanceInitialization();
		defineInstanceHelpers();

		defineClassClass(sourcePath, classClassWriter, classQualifiedSubclass, classQualifiedSuperclass);
		defineClassFields(classVariableNames, classInstanceVariableNames);
		defineClassInitialization();
		defineClassHelpers();

		defineClassRegistration();
	}

	private void defineClassHelpers() {
		System.out.println();
		MethodVisitor mv = classClassWriter.visitMethod(ACC_PUBLIC, "$new", "()Lst/redline/ProtoObject;", null, null);
		mv.visitCode();
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(14, l0);
		mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
		mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
		mv.visitInsn(DUP);
		mv.visitLdcInsn("new of ");
		mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V");
		mv.visitLdcInsn(Type.getType("L"+qualifiedSubclass+";"));
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
		Label l1 = new Label();
		mv.visitLabel(l1);
		mv.visitLineNumber(15, l1);
		mv.visitTypeInsn(NEW, qualifiedSubclass);
		mv.visitInsn(DUP);
		mv.visitMethodInsn(INVOKESPECIAL, qualifiedSubclass, "<init>", "()V");
		mv.visitInsn(ARETURN);
		Label l2 = new Label();
		mv.visitLabel(l2);
		mv.visitLocalVariable("this", "L"+classQualifiedSubclass+";", null, l0, l2, 0);
		mv.visitMaxs(4, 1);
		mv.visitEnd();

		if (subclass.equals("String") || subclass.equals("Symbol")) {
			System.out.println();
			mv = classClassWriter.visitMethod(ACC_PUBLIC, "$fromPrimitive", "(Ljava/lang/Object;)Lst/redline/ProtoObject;", null, null);
			mv.visitCode();
			l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(21, l0);
			mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
			mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
			mv.visitInsn(DUP);
			mv.visitLdcInsn("$fromPrimitive: ");
			mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V");
			mv.visitVarInsn(ALOAD, 1);
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
			l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(22, l1);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKEVIRTUAL, classQualifiedSubclass, "$new", "()Lst/redline/ProtoObject;");
			mv.visitTypeInsn(CHECKCAST, qualifiedSubclass);
			mv.visitVarInsn(ASTORE, 2);
			l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLineNumber(23, l2);
			mv.visitVarInsn(ALOAD, 2);
			Label l3 = new Label();
			mv.visitLabel(l3);
			mv.visitTypeInsn(NEW, "java/lang/StringBuffer");
			mv.visitInsn(DUP);
			mv.visitVarInsn(ALOAD, 1);
			Label l4 = new Label();
			mv.visitJumpInsn(IFNONNULL, l4);
			mv.visitLdcInsn("");
			Label l5 = new Label();
			mv.visitJumpInsn(GOTO, l5);
			mv.visitLabel(l4);
			mv.visitFrame(Opcodes.F_FULL, 3, new Object[] {classQualifiedSubclass, "java/lang/Object", qualifiedSubclass}, 3, new Object[] {qualifiedSubclass, l3, l3});
			mv.visitVarInsn(ALOAD, 1);
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "toString", "()Ljava/lang/String;");
			mv.visitLabel(l5);
			mv.visitFrame(Opcodes.F_FULL, 3, new Object[] {classQualifiedSubclass, "java/lang/Object", qualifiedSubclass}, 4, new Object[] {qualifiedSubclass, l3, l3, "java/lang/String"});
			mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuffer", "<init>", "(Ljava/lang/String;)V");
			mv.visitFieldInsn(PUTFIELD, qualifiedSubclass, "primitiveValue", "Ljava/lang/StringBuffer;");
			Label l6 = new Label();
			mv.visitLabel(l6);
			mv.visitLineNumber(24, l6);
			mv.visitVarInsn(ALOAD, 2);
			mv.visitInsn(ARETURN);
			Label l7 = new Label();
			mv.visitLabel(l7);
			mv.visitLocalVariable("this", "L"+classQualifiedSubclass+";", null, l0, l7, 0);
			mv.visitLocalVariable("value", "Ljava/lang/Object;", null, l0, l7, 1);
			mv.visitLocalVariable("object", "L"+qualifiedSubclass+";", null, l2, l7, 2);
			mv.visitMaxs(4, 3);
			mv.visitEnd();
		}
	}

	private void defineInstanceHelpers() {
		System.out.println();
		if (subclass.equals("Class")) {
			MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "$class", "()Lst/redline/Behavior;", null, null);
			mv.visitCode();
			Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(25, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, "st/redline/Class", "metaclass", "Lst/redline/Metaclass;");
			mv.visitInsn(ARETURN);
			Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLocalVariable("this", "Lst/redline/Class;", null, l0, l1, 0);
			mv.visitMaxs(1, 1);
			mv.visitEnd();

			mv = classWriter.visitMethod(ACC_PUBLIC, "$superclass", "()Lst/redline/ProtoObject;", null, null);
			mv.visitCode();
			l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(36, l0);
			mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
			mv.visitLdcInsn("superclass() called in Class");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
			l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(37, l1);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, "st/redline/Class", "primitiveClass", "Ljava/lang/Class;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getSuperclass", "()Ljava/lang/Class;");
			mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "classForPrimitiveClass", "(Ljava/lang/Class;)Lst/redline/ProtoObject;");
			mv.visitInsn(ARETURN);
			Label l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLocalVariable("this", "Lst/redline/Class;", null, l0, l2, 0);
			mv.visitMaxs(2, 1);
			mv.visitEnd();

		} else {

			MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "$class", "()Lst/redline/Behavior;", null, null);
			mv.visitCode();
			Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(15, l0);
			mv.visitFieldInsn(GETSTATIC, qualifiedSubclass, "$class", "L"+classQualifiedSubclass+";");
			mv.visitInsn(ARETURN);
			Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLocalVariable("this", "L"+qualifiedSubclass+";", null, l0, l1, 0);
			mv.visitMaxs(1, 1);
			mv.visitEnd();

			mv = classWriter.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, "$class", "()Lst/redline/ProtoObject;", null, null);
			mv.visitCode();
			l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(1, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKEVIRTUAL, qualifiedSubclass, "$class", "()Lst/redline/Behavior;");
			mv.visitInsn(ARETURN);
			mv.visitMaxs(1, 1);
			mv.visitEnd();

			if (subclass.equals("Metaclass")) {
				mv = classWriter.visitMethod(ACC_PUBLIC, "$superclass", "()Lst/redline/ProtoObject;", null, null);
				mv.visitCode();
				l0 = new Label();
				mv.visitLabel(l0);
				mv.visitLineNumber(27, l0);
				mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
				mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
				mv.visitInsn(DUP);
				mv.visitLdcInsn("Metaclass.$superclass() super= ");
				mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V");
				mv.visitVarInsn(ALOAD, 0);
				mv.visitFieldInsn(GETFIELD, "st/redline/Metaclass", "classWeAreMetaFor", "Lst/redline/Class;");
				mv.visitFieldInsn(GETFIELD, "st/redline/Class", "primitiveClass", "Ljava/lang/Class;");
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getSuperclass", "()Ljava/lang/Class;");
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
				l1 = new Label();
				mv.visitLabel(l1);
				mv.visitLineNumber(28, l1);
				mv.visitVarInsn(ALOAD, 0);
				mv.visitFieldInsn(GETFIELD, "st/redline/Metaclass", "classWeAreMetaFor", "Lst/redline/Class;");
				mv.visitFieldInsn(GETFIELD, "st/redline/Class", "primitiveClass", "Ljava/lang/Class;");
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getSuperclass", "()Ljava/lang/Class;");
				mv.visitVarInsn(ASTORE, 1);
				Label l2 = new Label();
				mv.visitLabel(l2);
				mv.visitLineNumber(29, l2);
				mv.visitVarInsn(ALOAD, 1);
				mv.visitLdcInsn(Type.getType("Lst/redline/ProtoObject;"));
				Label l3 = new Label();
				mv.visitJumpInsn(IF_ACMPNE, l3);
				Label l4 = new Label();
				mv.visitLabel(l4);
				mv.visitLineNumber(30, l4);
				mv.visitFieldInsn(GETSTATIC, "st/redline/Class", "$class", "Lst/redline/Class$mClass;");
				mv.visitInsn(ARETURN);
				mv.visitLabel(l3);
				mv.visitLineNumber(31, l3);
				mv.visitFrame(Opcodes.F_APPEND,1, new Object[] {"java/lang/Class"}, 0, null);
				mv.visitVarInsn(ALOAD, 1);
				mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "classForPrimitiveClass", "(Ljava/lang/Class;)Lst/redline/ProtoObject;");
				mv.visitTypeInsn(CHECKCAST, "st/redline/Behavior");
				mv.visitVarInsn(ASTORE, 2);
				Label l5 = new Label();
				mv.visitLabel(l5);
				mv.visitLineNumber(32, l5);
				mv.visitVarInsn(ALOAD, 2);
				Label l6 = new Label();
				mv.visitJumpInsn(IFNONNULL, l6);
				mv.visitInsn(ACONST_NULL);
				Label l7 = new Label();
				mv.visitJumpInsn(GOTO, l7);
				mv.visitLabel(l6);
				mv.visitFrame(Opcodes.F_APPEND,1, new Object[] {"st/redline/Behavior"}, 0, null);
				mv.visitVarInsn(ALOAD, 2);
				mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/Behavior", "$class", "()Lst/redline/Behavior;");
				mv.visitLabel(l7);
				mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, new Object[] {"st/redline/Behavior"});
				mv.visitInsn(ARETURN);
				Label l8 = new Label();
				mv.visitLabel(l8);
				mv.visitLocalVariable("this", "Lst/redline/Metaclass;", null, l0, l8, 0);
				mv.visitLocalVariable("aClass", "Ljava/lang/Class;", null, l2, l8, 1);
				mv.visitLocalVariable("aBehavior", "Lst/redline/Behavior;", null, l5, l8, 2);
				mv.visitMaxs(4, 3);
				mv.visitEnd();
			}

			if (subclass.equals("String") || subclass.equals("Symbol")) {
				mv = classWriter.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null);
				mv.visitCode();
				l0 = new Label();
				mv.visitLabel(l0);
				mv.visitLineNumber(10, l0);
				mv.visitVarInsn(ALOAD, 0);
				mv.visitFieldInsn(GETFIELD, qualifiedSubclass, "primitiveValue", "Ljava/lang/StringBuffer;");
				mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuffer", "toString", "()Ljava/lang/String;");
				mv.visitInsn(ARETURN);
				l1 = new Label();
				mv.visitLabel(l1);
				mv.visitLocalVariable("this", "L"+qualifiedSubclass+";", null, l0, l1, 0);
				mv.visitMaxs(1, 1);
				mv.visitEnd();
			}

			if (subclass.equals("BlockWithAnswer") || subclass.equals("BlockWithoutAnswer")) {
				// create each permutation of value method so subclass can implement.
				String signature = "value";
				for (int i = 0; i < METHOD_SIGNATURE.length; i++) {
					mv = classWriter.visitMethod(ACC_PUBLIC, signature, METHOD_SIGNATURE[i], null, null);
					mv.visitCode();
					mv.visitTypeInsn(NEW, "java/lang/IllegalStateException");
					mv.visitInsn(DUP);
					mv.visitLdcInsn("Subclass should implement.");
					mv.visitMethodInsn(INVOKESPECIAL, "java/lang/IllegalStateException", "<init>", "(Ljava/lang/String;)V");
					mv.visitInsn(ATHROW);
					mv.visitMaxs(1, 1);
					mv.visitEnd();
					signature = (i + 1 == 1) ? signature + ":" : signature + "value:";
				}
			}
		}
	}

	private void defineClassRegistration() {
		System.out.println();
		MethodVisitor mv = classWriter.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null);
		mv.visitCode();
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(12, l0);
		mv.visitTypeInsn(NEW, classQualifiedSubclass);
		mv.visitInsn(DUP);
		mv.visitLdcInsn(Type.getType("L"+qualifiedSubclass+";"));
		mv.visitMethodInsn(INVOKESPECIAL, classQualifiedSubclass, "<init>", "(Ljava/lang/Class;)V");
		mv.visitFieldInsn(PUTSTATIC, qualifiedSubclass, "$class", "L"+classQualifiedSubclass+";");
		Label l1 = new Label();
		mv.visitLabel(l1);
		mv.visitLineNumber(3, l1);
		mv.visitInsn(RETURN);
		mv.visitMaxs(3, 0);
		mv.visitEnd();
	}

	private void defineClassInitialization() {
		System.out.println();
		MethodVisitor mv = classClassWriter.visitMethod(ACC_PUBLIC, "<init>", "(Ljava/lang/Class;)V", null, null);
		mv.visitCode();
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(8, l0);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 1);
		mv.visitMethodInsn(INVOKESPECIAL, classQualifiedSuperclass, "<init>", "(Ljava/lang/Class;)V");
		Label l1 = new Label();
		mv.visitLabel(l1);
		mv.visitLineNumber(9, l1);
		mv.visitInsn(RETURN);
		Label l2 = new Label();
		mv.visitLabel(l2);
		mv.visitLocalVariable("this", "L"+classQualifiedSubclass+";", null, l0, l2, 0);
		mv.visitLocalVariable("aClass", "Ljava/lang/Class;", null, l0, l2, 1);
		mv.visitMaxs(2, 2);
		mv.visitEnd();
	}

	private void defineInstanceInitialization() {
		System.out.println();
		if (subclass.equals("Class")) {
			MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "<init>", "(Ljava/lang/Class;)V", null, null);
			mv.visitCode();
			Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(17, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKESPECIAL, "st/redline/ClassDescription", "<init>", "()V");
			Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(18, l1);
			mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
			mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
			mv.visitInsn(DUP);
			mv.visitLdcInsn("Constructing class with ");
			mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V");
			mv.visitVarInsn(ALOAD, 1);
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
			Label l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLineNumber(19, l2);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(PUTFIELD, "st/redline/Class", "primitiveClass", "Ljava/lang/Class;");
			Label l3 = new Label();
			mv.visitLabel(l3);
			mv.visitLineNumber(20, l3);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitTypeInsn(NEW, "st/redline/Metaclass");
			mv.visitInsn(DUP);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKESPECIAL, "st/redline/Metaclass", "<init>", "(Lst/redline/Class;)V");
			mv.visitFieldInsn(PUTFIELD, "st/redline/Class", "metaclass", "Lst/redline/Metaclass;");
			Label l4 = new Label();
			mv.visitLabel(l4);
			mv.visitLineNumber(21, l4);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/Class", "methodsFrom", "(Ljava/lang/Class;)Ljava/util/Map;");
			mv.visitFieldInsn(PUTFIELD, "st/redline/Class", "methodDictionary", "Ljava/util/Map;");
			Label l5 = new Label();
			mv.visitLabel(l5);
			mv.visitLineNumber(22, l5);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "register", "(Ljava/lang/Class;Lst/redline/ProtoObject;)V");
			Label l6 = new Label();
			mv.visitLabel(l6);
			mv.visitLineNumber(23, l6);
			mv.visitInsn(RETURN);
			Label l7 = new Label();
			mv.visitLabel(l7);
			mv.visitLocalVariable("this", "Lst/redline/Class;", null, l0, l7, 0);
			mv.visitLocalVariable("aClass", "Ljava/lang/Class;", null, l0, l7, 1);
			mv.visitMaxs(4, 2);
			mv.visitEnd();

		} else if (subclass.equals("Metaclass")) {

			MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/Class;)V", null, null);
			mv.visitCode();
			Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(16, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKESPECIAL, "st/redline/ClassDescription", "<init>", "()V");
			Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(17, l1);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(PUTFIELD, "st/redline/Metaclass", "classWeAreMetaFor", "Lst/redline/Class;");
			Label l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLineNumber(18, l2);
			mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
			mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
			mv.visitInsn(DUP);
			mv.visitLdcInsn("Constructing metaclass with ");
			mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "(Ljava/lang/String;)V");
			mv.visitVarInsn(ALOAD, 1);
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(Ljava/lang/Object;)Ljava/lang/StringBuilder;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
			Label l3 = new Label();
			mv.visitLabel(l3);
			mv.visitLineNumber(19, l3);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;");
			mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/Metaclass", "methodsFrom", "(Ljava/lang/Class;)Ljava/util/Map;");
			mv.visitFieldInsn(PUTFIELD, "st/redline/Metaclass", "methodDictionary", "Ljava/util/Map;");
			Label l4 = new Label();
			mv.visitLabel(l4);
			mv.visitLineNumber(20, l4);
			mv.visitInsn(RETURN);
			Label l5 = new Label();
			mv.visitLabel(l5);
			mv.visitLocalVariable("this", "Lst/redline/Metaclass;", null, l0, l5, 0);
			mv.visitLocalVariable("aClass", "Lst/redline/Class;", null, l0, l5, 1);
			mv.visitMaxs(4, 2);
			mv.visitEnd();

		} else {

			MethodVisitor mv = classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
			mv.visitCode();
			Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(3, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKESPECIAL, qualifiedSuperclass, "<init>", "()V");
			mv.visitInsn(RETURN);
			Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLocalVariable("this", "L"+qualifiedSubclass+";", null, l0, l1, 0);
			mv.visitMaxs(1, 1);
			mv.visitEnd();
		}
	}

	private void defineClassFields(String[] classVariableNames, String[] classInstanceVariableNames) {
		defineFields(classClassWriter, classVariableNames);
		defineFields(classClassWriter, classInstanceVariableNames);
	}

	private void defineInstanceFields(String[] instanceVariableNames) {
		FieldVisitor fv = classWriter.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, "$class", "L"+classQualifiedSubclass+";", null, null);
		fv.visitEnd();

		defineFields(classWriter, instanceVariableNames);

		if (subclass.equals("Class")) {
			fv = classWriter.visitField(ACC_PROTECTED, "metaclass", "Lst/redline/Metaclass;", null, null);
			fv.visitEnd();
			fv = classWriter.visitField(ACC_PUBLIC, "primitiveClass", "Ljava/lang/Class;", null, null);
			fv.visitEnd();
		} else if (subclass.equals("Metaclass")) {
			fv = classWriter.visitField(ACC_PRIVATE, "classWeAreMetaFor", "Lst/redline/Class;", null, null);
			fv.visitEnd();
		} else if (subclass.equals("String") || subclass.equals("Symbol")) {
			fv = classWriter.visitField(ACC_PUBLIC, "primitiveValue", "Ljava/lang/StringBuffer;", null, null);
			fv.visitEnd();
		}
	}

	private void defineFields(ClassWriter classWriter, String[] fields) {
		for (String field : fields) {
			FieldVisitor fv = classWriter.visitField(ACC_PUBLIC, field, "Lst/redline/ProtoObject;", null, null);
			fv.visitEnd();
		}
	}

	private void defineInstanceClass(String sourcePath, ClassWriter classWriter, String subclass, String superclass) {
		System.out.println();
		classWriter.visit(V1_5, ACC_PUBLIC + ACC_SUPER, subclass, null, superclass, null);
		classWriter.visitSource(subclass+".st", null);
		classWriter.visitInnerClass(qualifiedSubclass + "$mClass", qualifiedSubclass, "mClass", ACC_PUBLIC + ACC_STATIC);
	}

	private void defineClassClass(String sourcePath, ClassWriter classWriter, String subclass, String superclass) {
		System.out.println();
		classWriter.visit(V1_6, ACC_PUBLIC + ACC_SUPER, classQualifiedSubclass, null, classQualifiedSuperclass, null);
		classWriter.visitSource(subclass+".st", null);
		classWriter.visitInnerClass(classQualifiedSubclass, qualifiedSubclass, "mClass", ACC_PUBLIC + ACC_STATIC);
		classWriter.visitInnerClass(classQualifiedSuperclass, qualifiedSuperclass, "mClass", ACC_PUBLIC + ACC_STATIC);
	}

	public List<RawClass> definedClassBytes() {
		classWriter.visitEnd();
		classClassWriter.visitEnd();
		List<RawClass> classes = new ArrayList<RawClass>();
		classes.add(new RawClass(subclass, classWriter.toByteArray()));
		classes.add(new RawClass(subclass + "$mClass", classClassWriter.toByteArray()));
		classes.addAll(blocksAndBlockAnswers);
		return classes;
	}

//	private void defineClassConstructor(ClassDefinition classDefinition) {
//		MethodVisitor mv = classWriter.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null);
//		mv.visitCode();
////		defineClassInitialization(mv, classDefinition);
////		emitUnarySend(mv, classDefinition.unarySend());
////		emitKeywordSend(mv, classDefinition.keywordSends());
////		mv.visitInsn(POP);
//		mv.visitInsn(RETURN);
//		mv.visitMaxs(1, 1);
//		mv.visitEnd();
//	}

	private void emitMessage(MethodVisitor mv, String message) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(1, l0);
		mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
		mv.visitLdcInsn(message);
		mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V");
	}

	private void emitKeywordSend(MethodVisitor mv, List<KeywordSend> keywordSends) {
		emitKeywordSendSignature(mv, keywordSends);
		emitKeywordSendArguments(mv, keywordSends);
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(keywordSends.get(0).keyword().beginLine, l0);
		if (isSendToSuper()) {
			clearSendToSuper();
			mv.visitLdcInsn(currentMethodName);
			mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$superSend", SUPER_SEND_SIGNATURE[keywordSends.size()]);
		} else {
			mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$send", SEND_SIGNATURE[keywordSends.size()]);
		}
	}

	private void emitKeywordSendArguments(MethodVisitor mv, List<KeywordSend> keywordSends) {
		for (KeywordSend keywordSend : keywordSends)
			for (MessageSend messageSend : keywordSend.arguments())
				emitMessageSend(mv, messageSend);
	}

	private void emitKeywordSendSignature(MethodVisitor mv, List<KeywordSend> keywordSends) {
		StringBuffer keywords = new StringBuffer(128);
		for (KeywordSend keywordSend : keywordSends)
			keywords.append(keywordSend.keyword().toString());
		mv.visitLdcInsn(keywords.toString());
	}

	private void emitMessageSend(MethodVisitor mv, MessageSend messageSend) {
		if (messageSend instanceof UnarySend) {
			emitUnarySend(mv, (UnarySend) messageSend);
		} else {
			throw new RuntimeException("Need to handle MessageSend of type: " + messageSend.getClass());
		}
	}

	private void emitUnarySend(MethodVisitor mv, UnarySend unarySend) {
		emitPrimary(mv, unarySend.primary());
		for (Token selector : unarySend.selectors())
			emitUnaryCall(mv, selector);
	}

	private void emitUnaryCall(MethodVisitor mv, Token selector) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(selector.beginLine, l0);
		mv.visitLdcInsn(selector.toString());
		if (isSendToSuper()) {
			clearSendToSuper();
			mv.visitLdcInsn(currentMethodName);
			mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$superSend", SUPER_SEND_SIGNATURE[0]);
		} else {
			mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$send", SEND_SIGNATURE[0]);
		}
	}

	private void clearSendToSuper() {
		superSend.pop();
	}

	private boolean isSendToSuper() {
		if (superSend.empty())
			return false;
		return superSend.peek().booleanValue();
	}

	private void emitBinarySends(MethodVisitor mv, List<BinarySend> binarySends) {
		throw new RuntimeException("TODO - emitBinarySend(S)");
	}

	private void emitBinarySend(MethodVisitor mv, BinarySend binarySend) {
		throw new RuntimeException("TODO - emitBinarySend");
	}

	private void emitPrimary(MethodVisitor mv, Primary primary) {
		if (primary instanceof PrimaryVariable) {
			emitPrimary(mv, (PrimaryVariable) primary);
		} else if (primary instanceof PrimaryLiteral) {
			emitPrimary(mv, (PrimaryLiteral) primary);
		} else if (primary instanceof PrimaryBlock) {
			emitPrimary(mv, (PrimaryBlock) primary);
		} else {
			throw new RuntimeException("Need to handle PRIMARY of type: " + primary.getClass());
		}
	}

	private void emitPrimary(MethodVisitor mv, PrimaryBlock primaryBlock) {
		Block block = primaryBlock.block();
		emitBlockInstantiation(mv, block);
		emitBlockClassAndException(block);
	}

	private void emitBlockClassAndException(Block block) {
		System.out.println();
		String outerClass = block.definedInClassMethod() ? classQualifiedSubclass : qualifiedSubclass;
		String blockName = outerClass + "$b" + block.identifier();
		String blockClassName = (block.definedInClassMethod() ? subclass + "$mClass" : subclass) + "$b" + block.identifier();
		boolean blockHasAnsweredExpression = block.hasAnsweredExpression();
		String subclassedBlock = blockHasAnsweredExpression ? "st/redline/BlockWithAnswer" : "st/redline/BlockWithoutAnswer";

		block.className(blockClassName);
		block.answerClassName(blockClassName+"Answer");

		ClassWriter cw = new TracingClassWriter(ClassWriter.COMPUTE_MAXS);
		cw.visit(V1_6, ACC_SUPER, blockName, null, subclassedBlock, null);
		cw.visitSource(subclass+".st", null);
		cw.visitOuterClass(outerClass, currentMethodName, "()L"+subclassedBlock+";");
		cw.visitInnerClass(blockName, null, null, 0);

		FieldVisitor fv = cw.visitField(ACC_FINAL + ACC_SYNTHETIC, "this$0", "L"+outerClass+";", null, null);
		fv.visitEnd();

		MethodVisitor mv = cw.visitMethod(0, "<init>", "(L"+outerClass+";)V", null, null);
		mv.visitCode();
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(1, l0);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitVarInsn(ALOAD, 1);
		mv.visitFieldInsn(PUTFIELD, blockName, "this$0", "L"+outerClass+";");
		Label l1 = new Label();
		mv.visitLabel(l1);
		mv.visitLineNumber(31, l1);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKESPECIAL, subclassedBlock, "<init>", "()V");
		mv.visitInsn(RETURN);
		Label l2 = new Label();
		mv.visitLabel(l2);
		mv.visitLocalVariable("this", "L"+blockName+";", null, l0, l2, 0);
		mv.visitMaxs(2, 2);
		mv.visitEnd();

		// emit value method for block
		emitBlockValueMethod(cw, block, blockHasAnsweredExpression);

		// add to list of blocksAndBlockAnswers.
		cw.visitEnd();
		blocksAndBlockAnswers.add(new RawClass(blockClassName, cw.toByteArray()));

		if (blockHasAnsweredExpression) {
			System.out.println();

			cw.visit(V1_6, ACC_PUBLIC + ACC_SUPER, blockName+"Answer", null, "java/lang/Error", null);
			cw.visitSource(subclass+".st", null);

			fv = cw.visitField(ACC_PRIVATE + ACC_FINAL, "answer", "Lst/redline/ProtoObject;", null, null);
			fv.visitEnd();

			mv = cw.visitMethod(ACC_PUBLIC, "<init>", "(Lst/redline/ProtoObject;)V", null, null);
			mv.visitCode();
			l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(5, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Error", "<init>", "()V");
			l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(6, l1);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitVarInsn(ALOAD, 1);
			mv.visitFieldInsn(PUTFIELD, blockName+"Answer", "answer", "Lst/redline/ProtoObject;");
			l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLineNumber(7, l2);
			mv.visitInsn(RETURN);
			mv.visitMaxs(2, 2);
			mv.visitEnd();

			mv = cw.visitMethod(ACC_PUBLIC, "answer", "()Lst/redline/ProtoObject;", null, null);
			mv.visitCode();
			l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(9, l0);
			mv.visitVarInsn(ALOAD, 0);
			mv.visitFieldInsn(GETFIELD, blockName+"Answer", "answer", "Lst/redline/ProtoObject;");
			mv.visitInsn(ARETURN);
			mv.visitMaxs(1, 1);
			mv.visitEnd();

			cw.visitEnd();
			blocksAndBlockAnswers.add(new RawClass(blockClassName+"Answer", cw.toByteArray()));
		}
	}

	private void emitBlockValueMethod(ClassWriter cw, Block block, boolean blockHasAnsweredExpression) {
		System.out.println();
		System.out.println("Block value method\n");

		// NOTE: We change the state of instance variables in the Encoder here,
		// then put them back when we are done.
		String tempClassQualifiedSubclass = classQualifiedSubclass;
		String tempQualifiedSubclass = qualifiedSubclass; 
		currentBlock.push(block);

		MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "value", METHOD_SIGNATURE[0], null, null);
		mv.visitCode();

		emitMessage(mv, "BLOCK VALUE METHOD CALLED " + (blockHasAnsweredExpression ? "^" : ""));
		if (block.statements() != null)
			emitStatements(mv, block.statements());

		// default answer of a method is the receiver.
		mv.visitVarInsn(ALOAD, 0);
		mv.visitInsn(ARETURN);

		mv.visitMaxs(1, 1);
		mv.visitEnd();

		currentBlock.pop();
		classQualifiedSubclass = tempClassQualifiedSubclass;
		qualifiedSubclass = tempQualifiedSubclass;

		System.out.println();
	}

	private void emitBlockInstantiation(MethodVisitor mv, Block block) {
		String outerClassName = block.definedInClassMethod() ? classQualifiedSubclass : qualifiedSubclass;
		String blockClassName = outerClassName + "$b" + block.identifier();
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(block.lineNumber(), l0);
		mv.visitTypeInsn(NEW, blockClassName);
		mv.visitInsn(DUP);
		mv.visitVarInsn(ALOAD, 0);
		mv.visitMethodInsn(INVOKESPECIAL, blockClassName, "<init>", "(L"+outerClassName+";)V");
	}

	private void emitPrimary(MethodVisitor mv, PrimaryVariable primaryVariable) {
		Variable variable = primaryVariable.variable();
		int lineNumber = variable.lineNumber();
		if (variable instanceof ReferenceToVariable)
			variable = ((ReferenceToVariable) variable).referencedVariable();

		if (variable.isClass()) {
			emitClassLoopkup(mv, variable, lineNumber);
		} else if (variable instanceof Argument) {
			emitArgumentReference(mv, (Argument) variable, lineNumber);
		} else if (variable instanceof InstanceField) {
			emitFieldLoad(mv, (InstanceField) variable, lineNumber);
		} else if (variable instanceof ClassField) {
			emitFieldLoad(mv, (ClassField) variable, lineNumber);
		} else {
			throw new RuntimeException("Need to handle other types of VARIABLE: " + variable.getClass() + " " + variable.toString());
		}
	}

	private void emitFieldLoad(MethodVisitor mv, ClassField classField, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitFieldInsn(GETSTATIC, qualifiedSubclass, "$class", "L"+classQualifiedSubclass+";");
		mv.visitFieldInsn(GETFIELD, classQualifiedSubclass, classField.toString(), "Lst/redline/ProtoObject;");
	}

	private void emitFieldLoad(MethodVisitor mv, InstanceField instanceField, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitVarInsn(ALOAD, 0);
		if (instanceField.onInstance())
			mv.visitFieldInsn(GETFIELD, qualifiedSubclass, instanceField.toString(), "Lst/redline/ProtoObject;");
		else
			mv.visitFieldInsn(GETFIELD, classQualifiedSubclass, instanceField.toString(), "Lst/redline/ProtoObject;");
	}

	private void emitArgumentReference(MethodVisitor mv, Argument variable, int lineNumber) {
		if (variable.offset() == 0)
			throw new RuntimeException("Argument offset unexpectedly zero (0)");
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitVarInsn(ALOAD, variable.offset());
	}

	private void emitPrimary(MethodVisitor mv, PrimaryLiteral primaryLiteral) {
		Literal literal = primaryLiteral.literal();
		if (literal instanceof ArraySymbolLiteral) {
			emitLiteral(mv, (ArraySymbolLiteral) literal);
		} else if (literal instanceof SymbolLiteral) {
			emitLiteral(mv, (SymbolLiteral) literal);
		} else if (literal instanceof StringLiteral) {
			emitLiteral(mv, (StringLiteral) literal);
		} else if (literal instanceof SpecialLiteral) {
			emitLiteral(mv, (SpecialLiteral) literal);
		} else {
			throw new RuntimeException("Need to handle other types of LITERAL: " + literal.getClass());
		}
	}

	private void emitLiteral(MethodVisitor mv, SpecialLiteral specialLiteral) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(specialLiteral.lineNumber(), l0);
		String value = specialLiteral.toString();
		if (value.equals("self")) {
			mv.visitVarInsn(ALOAD, 0);
		} else if (value.equals("nil")) {
			mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "$nil", "()Lst/redline/ProtoObject;");
		} else if (value.equals("true")) {
			mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "$true", "()Lst/redline/ProtoObject;");
		} else if (value.equals("false")) {
			mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "$false", "()Lst/redline/ProtoObject;");
		} else if (value.equals("super")) {
			superSend.push(Boolean.TRUE);
		} else {
			throw new RuntimeException("Need to handle other SPECIAL LITERAL: " + value);
		}
	}

	private void emitLiteral(MethodVisitor mv, ArraySymbolLiteral arraySymbolLiteral) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(arraySymbolLiteral.lineNumber(), l0);
		mv.visitLdcInsn("Array");
		mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "classNamed", "(Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitLdcInsn("new");
		mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$send", "(Ljava/lang/String;)Lst/redline/ProtoObject;");
		if (arraySymbolLiteral.hasElements())
			throw new RuntimeException("TODO - handle array symbol literal - with elements");
	}

	private void emitLiteral(MethodVisitor mv, SymbolLiteral symbolLiteral) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(symbolLiteral.lineNumber(), l0);
		mv.visitLdcInsn("Symbol");
		mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "classNamed", "(Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitLdcInsn(symbolLiteral.toString());
		mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$fromPrimitive", "(Ljava/lang/Object;)Lst/redline/ProtoObject;");
	}

	private void emitLiteral(MethodVisitor mv, StringLiteral stringLiteral) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(stringLiteral.lineNumber(), l0);
		mv.visitLdcInsn("String");
		mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "classNamed", "(Ljava/lang/String;)Lst/redline/ProtoObject;");
		mv.visitLdcInsn(stringLiteral.toString());
		mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$fromPrimitive", "(Ljava/lang/Object;)Lst/redline/ProtoObject;");
	}

	private void emitClassLoopkup(MethodVisitor mv, Variable variable, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitLdcInsn(variable.toString());
		mv.visitMethodInsn(INVOKESTATIC, "st/redline/Smalltalk", "classNamed", "(Ljava/lang/String;)Lst/redline/ProtoObject;");
	}

	public void defineMethods(String sourcePath, int classDefinitionLineNumber, List<Method> methods, boolean classMethods) {
		for (Method method : methods)
			defineMethod(sourcePath, classDefinitionLineNumber, method, classMethods);
	}

	private void defineMethod(String sourcePath, int classDefinitionLineNumber, Method method, boolean classMethods) {
		System.out.println();
		MessagePattern messagePattern = method.messagePattern();
		String methodName = messagePattern.pattern();
		currentMethodName = methodName;
		MethodVisitor mv;
		if (classMethods)
			mv = classClassWriter.visitMethod(ACC_PUBLIC, methodName, METHOD_SIGNATURE[messagePattern.argumentCount()], null, null);
		else
			mv = classWriter.visitMethod(ACC_PUBLIC, methodName, METHOD_SIGNATURE[messagePattern.argumentCount()], null, null);
		mv.visitCode();
		if (method.pragmas()[0] != null)
			emitPragmas(mv, method.pragmas()[0]);
		if (method.temporaries() != null)
			emitTemporaries(mv, method.temporaries());
		if (method.pragmas()[1] != null)
			emitPragmas(mv, method.pragmas()[1]);
		if (method.statements() != null) {
			emitStatements(mv, method.statements());
		}

		// default answer of a method is the receiver.
		mv.visitVarInsn(ALOAD, 0);
		mv.visitInsn(ARETURN);

		mv.visitMaxs(1, 1);
		mv.visitEnd();
	}

	private void emitStatements(MethodVisitor mv, Statements statements) {
		for (Expression expression : statements.expressions())
			emitExpression(mv, expression);
	}

	private void emitExpression(MethodVisitor mv, Expression expression) {
		int superSendsBeforeCount = superSend.size();
		if (expression instanceof AssignmentExpression) {
			emitExpression(mv, (AssignmentExpression) expression);
		} else {
			if (expression.isPrimary())
				emitPrimary(mv, expression.primary());
			emitCascade(mv, expression.cascade());

			// answer top of stack if this is ^ <expression> 
			if (expression.isAnswered()) {
				// throw answer if in block and block has ^ <expression>
				if (!currentBlock.isEmpty())
					System.out.println("*** SHOULD THROW ANSWER " + currentBlock.peek().answerClassName());
				mv.visitInsn(ARETURN);
			}
		}
		if (superSendsBeforeCount != superSend.size())
			throw new RuntimeException("A 'super' send was not handled.");
	}

	private void emitExpression(MethodVisitor mv, AssignmentExpression expression) {
		Variable variable = expression.variable();
		int lineNumber = variable.lineNumber();

		if (variable instanceof ReferenceToVariable)
			variable = ((ReferenceToVariable) variable).referencedVariable();

		// prologue to field store. See additional handling below.
		if (variable instanceof InstanceField) {
			mv.visitVarInsn(ALOAD, 0);
		} else if (variable instanceof ClassField) {
			mv.visitFieldInsn(GETSTATIC, qualifiedSubclass, "$class", "L"+classQualifiedSubclass+";");
		}

		Expression rightOfAssignment = expression.expression();
		emitExpression(mv, rightOfAssignment);
		if (rightOfAssignment instanceof AssignmentExpression)
			throw new RuntimeException("Need to support multiple assignment. v1 := v2 := etc.");

		if (variable instanceof Argument) {
			emitStore(mv, (Argument) variable, lineNumber);
		} else if (variable instanceof Temporary) {
			emitStore(mv, (Temporary) variable, lineNumber);
		} else if (variable instanceof InstanceField) {
			emitStore(mv, (InstanceField) variable, lineNumber);
		} else if (variable instanceof ClassField) {
			emitStore(mv, (ClassField) variable, lineNumber);
		} else {
			throw new RuntimeException("Need to handle AssignmentExpression Variable: " + variable.getClass());
		}
	}

	private void emitStore(MethodVisitor mv, ClassField classField, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitFieldInsn(PUTFIELD, classQualifiedSubclass, classField.toString(), "Lst/redline/ProtoObject;");
	}

	private void emitStore(MethodVisitor mv, InstanceField instanceField, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		if (instanceField.onInstance())
			mv.visitFieldInsn(PUTFIELD, qualifiedSubclass, instanceField.toString(), "Lst/redline/ProtoObject;");
		else
			mv.visitFieldInsn(PUTFIELD, classQualifiedSubclass, instanceField.toString(), "Lst/redline/ProtoObject;");
	}

	private void emitStore(MethodVisitor mv, Argument argument, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitVarInsn(ASTORE, argument.offset());
	}

	private void emitStore(MethodVisitor mv, Temporary temporary, int lineNumber) {
		Label l0 = new Label();
		mv.visitLabel(l0);
		mv.visitLineNumber(lineNumber, l0);
		mv.visitVarInsn(ASTORE, temporary.offset());
	}

	private void emitCascade(MethodVisitor mv, Cascade cascade) {
		emitUnarySend(mv, cascade.unarySend());
		if (cascade.hasBinarySends())
			emitBinarySends(mv, cascade.binarySends());
		if (cascade.hasKeywordSends())
			emitKeywordSend(mv, cascade.keywordSends());
		if (cascade.hasCascadedSends()) {
			for (Object send : cascade.cascadedSends()) {
				mv.visitInsn(DUP);   // duplicate top of stack as receiver (A)
				emitCascadeSend(mv, send);
				mv.visitInsn(POP);   // pop result (back to original receiver (A))
			}
		}
	}

	private void emitCascadeSend(MethodVisitor mv, Object send) {
		if (send instanceof Token)
			emitUnaryCall(mv, (Token) send);
		else if (send instanceof BinarySend)
			emitBinarySend(mv, (BinarySend) send);
		else if (send instanceof List)
			emitKeywordSend(mv, (List<KeywordSend>) send);
	}

	private void emitTemporaries(MethodVisitor mv, List<Variable> variables) {
		// We dont do anything on the declaration of a temporary.
		// Outputing for visual validation only, to ensure offset is after args
		// and if no args offset starts at 1.
		for (Variable variable : variables)
			System.out.println("temp var: " + variable + " @ " + variable.offset());
	}

	private void emitPragmas(MethodVisitor mv, Pragma pragma) {
		Primitive primitive = pragma.primitive();
		if (!primitive.hasSecondPart()) {
			Object firstPart = primitive.firstPart();
			if (firstPart instanceof Number)
				emitPrimitive(mv, (Number) firstPart);
		} else {
			throw new RuntimeException("Need to handle primitive with second part: " + primitive.firstPart() + " " + primitive.secondPart());
		}
	}

	private void emitPrimitive(MethodVisitor mv, Number primitiveIdentifier) {
		int index = Integer.parseInt(primitiveIdentifier.toString());
		switch (index) {
			// Behavior basicNew ...
			case 70:
				Label l0 = new Label();
				mv.visitLabel(l0);
				mv.visitLineNumber(primitiveIdentifier.lineNumber(), l0);
				mv.visitVarInsn(ALOAD, 0);
				mv.visitMethodInsn(INVOKEVIRTUAL, "st/redline/ProtoObject", "$new", "()Lst/redline/ProtoObject;");
				mv.visitInsn(ARETURN);
				break;
			default:
				throw new IllegalStateException("Unknown primitive identifier: " + index);
		}
	}

	private void emitNumber(MethodVisitor mv, int number) {
		switch (number) {
			case 0:
				mv.visitInsn(ICONST_0);
				break;
			case 1:
				mv.visitInsn(ICONST_1);
				break;
			case 2:
				mv.visitInsn(ICONST_2);
				break;
			case 3:
				mv.visitInsn(ICONST_3);
				break;
			case 4:
				mv.visitInsn(ICONST_4);
				break;
			case 5:
				mv.visitInsn(ICONST_5);
				break;
			default:
				if (number < 128)
					mv.visitIntInsn(BIPUSH, number);
				else
					mv.visitIntInsn(SIPUSH, number);
		}
	}
}
