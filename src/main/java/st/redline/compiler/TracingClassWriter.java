/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import org.objectweb.asm.*;

import java.io.PrintWriter;

public class TracingClassWriter extends ClassWriter {

	private final PrintWriter out;

	public TracingClassWriter(int computeMaxs, PrintWriter printWriter) {
		super(computeMaxs);
		this.out = printWriter;
	}

	@Override
	public void visit(int i, int i1, String s, String s1, String s2, String[] strings) {
		out.println("");
		out.println("visit(" + i + ", " + i1 + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + strings + ")");
		super.visit(i, i1, s, s1, s2, strings);
	}

	@Override
	public void visitSource(String s, String s1) {
		out.println("visitSource('" + s + "', '" + s1 + "')");
		super.visitSource(s, s1);
	}

	@Override
	public void visitOuterClass(String s, String s1, String s2) {
		out.println("visitOuterClass('" + s + "', '" + s1 + "', '" + s2 + "')");
		super.visitOuterClass(s, s1, s2);
	}

	@Override
	public AnnotationVisitor visitAnnotation(String s, boolean b) {
		out.println("visitAnnotation('" + s + "', " + b + ")");
		return super.visitAnnotation(s, b);
	}

	@Override
	public void visitAttribute(Attribute attribute) {
		out.println("visitAttribute(" + attribute + ")");
		super.visitAttribute(attribute);
	}

	@Override
	public void visitInnerClass(String s, String s1, String s2, int i) {
		out.println("visitInnerClass('" + s + "', '" + s1 + "', '" + s2 + "', " + i + ")");
		super.visitInnerClass(s, s1, s2, i);
	}

	@Override
	public FieldVisitor visitField(int i, String s, String s1, String s2, Object o) {
		out.println("visitField(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + o + ")");
		return super.visitField(i, s, s1, s2, o);
	}

	@Override
	public MethodVisitor visitMethod(int i, String s, String s1, String s2, String[] strings) {
		out.println("\nvisitMethod(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + strings + ")");
		return new TracingMethodVisitor(super.visitMethod(i, s, s1, s2, strings));
	}

	@Override
	public void visitEnd() {
		out.println("visitEnd()\n");
		super.visitEnd();
	}
}