package st.redline.compiler;

import org.objectweb.asm.*;

public class TracingClassWriter extends ClassWriter {

	public TracingClassWriter(int computeMaxs) {
		super(computeMaxs);
	}

	@Override
	public void visit(int i, int i1, String s, String s1, String s2, String[] strings) {
		System.out.println("\tvisit(" + i + ", " + i1 + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + strings + ")");
		super.visit(i, i1, s, s1, s2, strings);
	}

	@Override
	public void visitSource(String s, String s1) {
		System.out.println("\tvisitSource('" + s + "', '" + s1 + "')");
		super.visitSource(s, s1);
	}

	@Override
	public void visitOuterClass(String s, String s1, String s2) {
		System.out.println("\tvisitOuterClass('" + s + "', '" + s1 + "', '" + s2 + "')");
		super.visitOuterClass(s, s1, s2);
	}

	@Override
	public AnnotationVisitor visitAnnotation(String s, boolean b) {
		System.out.println("\tvisitAnnotation('" + s + "', " + b + ")");
		return super.visitAnnotation(s, b);
	}

	@Override
	public void visitAttribute(Attribute attribute) {
		System.out.println("\tvisitAttribute(" + attribute + ")");
		super.visitAttribute(attribute);
	}

	@Override
	public void visitInnerClass(String s, String s1, String s2, int i) {
		System.out.println("\tvisitInnerClass('" + s + "', '" + s1 + "', '" + s2 + "', " + i + ")");
		super.visitInnerClass(s, s1, s2, i);
	}

	@Override
	public FieldVisitor visitField(int i, String s, String s1, String s2, Object o) {
		System.out.println("\tvisitField(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + o + ")");
		return super.visitField(i, s, s1, s2, o);
	}

	@Override
	public MethodVisitor visitMethod(int i, String s, String s1, String s2, String[] strings) {
		System.out.println("\tvisitMethod(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + strings + ")");
		return new TracingMethodVisitor(super.visitMethod(i, s, s1, s2, strings));
	}

	@Override
	public void visitEnd() {
		System.out.println("\tvisitEnd()");
		super.visitEnd();
	}
}