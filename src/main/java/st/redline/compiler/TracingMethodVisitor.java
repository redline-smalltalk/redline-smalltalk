package st.redline.compiler;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

public class TracingMethodVisitor implements MethodVisitor {

	private final MethodVisitor methodVisitor;

	public TracingMethodVisitor(MethodVisitor methodVisitor) {
		this.methodVisitor = methodVisitor;
	}

	public AnnotationVisitor visitAnnotationDefault() {
		System.out.println("\tvisitAnnotationDefault()");
		return methodVisitor.visitAnnotationDefault();
	}

	public AnnotationVisitor visitAnnotation(String s, boolean b) {
		System.out.println("\tvisitAnnotation('" + s + "', " + b + ")");
		return methodVisitor.visitAnnotation(s, b);
	}

	public AnnotationVisitor visitParameterAnnotation(int i, String s, boolean b) {
		System.out.println("\tvisitParameterAnnotation(" + i + ", '" + s + "', " + b + ")");
		return methodVisitor.visitParameterAnnotation(i, s, b);
	}

	public void visitAttribute(Attribute attribute) {
		System.out.println("\tvisitAttribute(" + attribute + ")");
		methodVisitor.visitAttribute(attribute);
	}

	public void visitCode() {
		System.out.println("\tvisitCode()");
		methodVisitor.visitCode();
	}

	public void visitFrame(int i, int i1, Object[] objects, int i2, Object[] objects1) {
		System.out.println("\tvisitFrame(" + i + ", " + i1 + ", " + objects + ", " + i2 + ", " + objects1 + ")");
		methodVisitor.visitFrame(i, i1, objects, i2, objects1);
	}

	public void visitInsn(int i) {
		System.out.println("\tvisitInsn(" + i + ")");
		methodVisitor.visitInsn(i);
	}

	public void visitIntInsn(int i, int i1) {
		System.out.println("\tvisitIntInsn(" + i + ", " + i1 + ")");
		methodVisitor.visitIntInsn(i, i1);
	}

	public void visitVarInsn(int i, int i1) {
		System.out.println("\tvisitVarInsn(" + i + ", " + i1 + ")");
		methodVisitor.visitVarInsn(i, i1);
	}

	public void visitTypeInsn(int i, String s) {
		System.out.println("\tvisitTypeInsn(" + i + ", '" + s + "')");
		methodVisitor.visitTypeInsn(i, s);
	}

	public void visitFieldInsn(int i, String s, String s1, String s2) {
		System.out.println("\tvisitFieldInsn(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "')");
		methodVisitor.visitFieldInsn(i, s, s1, s2);
	}

	public void visitMethodInsn(int i, String s, String s1, String s2) {
		System.out.println("\tvisitMethodInsn(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "')");
		methodVisitor.visitMethodInsn(i, s, s1, s2);
	}

	public void visitJumpInsn(int i, Label label) {
		System.out.println("\tvisitJumpInsn(" + i + ", " + label + ")");
		methodVisitor.visitJumpInsn(i, label);
	}

	public void visitLabel(Label label) {
		System.out.println("\tvisitLabel(" + label + ")");
		methodVisitor.visitLabel(label);
	}

	public void visitLdcInsn(Object o) {
		System.out.println("\tvisitLdcInsn(" + o + ")");
		methodVisitor.visitLdcInsn(o);
	}

	public void visitIincInsn(int i, int i1) {
		System.out.println("\tvisitIincInsn(" + i + ", " + i1 + ")");
		methodVisitor.visitIincInsn(i, i1);
	}

	public void visitTableSwitchInsn(int i, int i1, Label label, Label[] labels) {
		System.out.println("\tvisitTableSwitchInsn(" + i + ", " + i1 + ", " + label + ", " + labels + ")");
		methodVisitor.visitTableSwitchInsn(i, i1, label, labels);
	}

	public void visitLookupSwitchInsn(Label label, int[] ints, Label[] labels) {
		System.out.println("\tvisitLookupSwitchInsn(" + label + ", " + ints + ", " + labels + ")");
		methodVisitor.visitLookupSwitchInsn(label, ints, labels);
	}

	public void visitMultiANewArrayInsn(String s, int i) {
		System.out.println("\tvisitMultiANewArrayInsn('" + s + "', " + i + ")");
		methodVisitor.visitMultiANewArrayInsn(s, i);
	}

	public void visitTryCatchBlock(Label label, Label label1, Label label2, String s) {
		System.out.println("\tvisitTryCatchBlock(" + label + ", " + label1 + ", " + label2 + ", '" + s + "')");
		methodVisitor.visitTryCatchBlock(label, label1, label2, s);
	}

	public void visitLocalVariable(String s, String s1, String s2, Label label, Label label1, int i) {
		System.out.println("\tvisitLocalVariable('" + s + "', '" + s1 + "', '" + s2 + "', " + label + ", " + label1 + ", " + i + ")");
		methodVisitor.visitLocalVariable(s, s1, s2, label, label1, i);
	}

	public void visitLineNumber(int i, Label label) {
		System.out.println("\tvisitLineNumber(" + i + ", " + label + ")");
		methodVisitor.visitLineNumber(i, label);
	}

	public void visitMaxs(int i, int i1) {
		System.out.println("\tvisitMaxs(" + i + ", " + i1 + ")");
		methodVisitor.visitMaxs(i, i1);
	}

	public void visitEnd() {
		System.out.println("\tvisitEnd()");
		methodVisitor.visitEnd();
	}
}
