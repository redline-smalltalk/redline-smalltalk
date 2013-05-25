/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.objectweb.asm.*;

import java.io.PrintWriter;

public class TracingClassVisitor extends ClassVisitor {

    private final PrintWriter out;
    private final ClassVisitor delegate;

    public TracingClassVisitor(ClassVisitor delegate, PrintWriter printWriter) {
        super(Opcodes.ASM4);
        this.delegate = delegate;
        this.out = printWriter;
    }

    public void visit(int i, int i1, String s, String s1, String s2, String[] strings) {
        out.println("");
        out.println("  visit(" + i + ", " + i1 + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + strings + ")");
        delegate.visit(i, i1, s, s1, s2, strings);
    }

    public void visitSource(String s, String s1) {
        out.println("  visitSource('" + s + "', '" + s1 + "')");
        delegate.visitSource(s, s1);
    }

    public void visitOuterClass(String s, String s1, String s2) {
        out.println("  visitOuterClass('" + s + "', '" + s1 + "', '" + s2 + "')");
        delegate.visitOuterClass(s, s1, s2);
    }

    public AnnotationVisitor visitAnnotation(String s, boolean b) {
        out.println("  visitAnnotation('" + s + "', " + b + ")");
        return delegate.visitAnnotation(s, b);
    }

    public void visitAttribute(Attribute attribute) {
        out.println("  visitAttribute(" + attribute + ")");
        delegate.visitAttribute(attribute);
    }

    public void visitInnerClass(String s, String s1, String s2, int i) {
        out.println("  visitInnerClass('" + s + "', '" + s1 + "', '" + s2 + "', " + i + ")");
        delegate.visitInnerClass(s, s1, s2, i);
    }

    public FieldVisitor visitField(int i, String s, String s1, String s2, Object o) {
        out.println("  visitField(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + o + ")");
        return delegate.visitField(i, s, s1, s2, o);
    }

    public MethodVisitor visitMethod(int i, String s, String s1, String s2, String[] strings) {
        out.println("\n  visitMethod(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "', " + strings + ")");
        return new TracingMethodVisitor(delegate.visitMethod(i, s, s1, s2, strings));
    }

    public void visitEnd() {
        out.println("  visitEnd()\n");
        delegate.visitEnd();
    }
}