/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package st.redline.smalltalk.interpreter;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Attribute;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TracingMethodVisitor implements MethodVisitor {

	final Logger log = LoggerFactory.getLogger(TracingMethodVisitor.class);

	private final MethodVisitor methodVisitor;

	public TracingMethodVisitor(MethodVisitor methodVisitor) {
		this.methodVisitor = methodVisitor;
	}

	public AnnotationVisitor visitAnnotationDefault() {
		log.info("visitAnnotationDefault()");
		return methodVisitor.visitAnnotationDefault();
	}

	public AnnotationVisitor visitAnnotation(String s, boolean b) {
		log.info("visitAnnotation('" + s + "', " + b + ")");
		return methodVisitor.visitAnnotation(s, b);
	}

	public AnnotationVisitor visitParameterAnnotation(int i, String s, boolean b) {
		log.info("visitParameterAnnotation(" + i + ", '" + s + "', " + b + ")");
		return methodVisitor.visitParameterAnnotation(i, s, b);
	}

	public void visitAttribute(Attribute attribute) {
		log.info("visitAttribute(" + attribute + ")");
		methodVisitor.visitAttribute(attribute);
	}

	public void visitCode() {
		log.info("visitCode()");
		methodVisitor.visitCode();
	}

	public void visitFrame(int i, int i1, Object[] objects, int i2, Object[] objects1) {
		log.info("visitFrame(" + i + ", " + i1 + ", " + objects + ", " + i2 + ", " + objects1 + ")");
		methodVisitor.visitFrame(i, i1, objects, i2, objects1);
	}

	public void visitInsn(int i) {
		log.info("visitInsn(" + i + ")");
		methodVisitor.visitInsn(i);
	}

	public void visitIntInsn(int i, int i1) {
		log.info("visitIntInsn(" + i + ", " + i1 + ")");
		methodVisitor.visitIntInsn(i, i1);
	}

	public void visitVarInsn(int i, int i1) {
		log.info("visitVarInsn(" + i + ", " + i1 + ")");
		methodVisitor.visitVarInsn(i, i1);
	}

	public void visitTypeInsn(int i, String s) {
		log.info("visitTypeInsn(" + i + ", '" + s + "')");
		methodVisitor.visitTypeInsn(i, s);
	}

	public void visitFieldInsn(int i, String s, String s1, String s2) {
		log.info("visitFieldInsn(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "')");
		methodVisitor.visitFieldInsn(i, s, s1, s2);
	}

	public void visitMethodInsn(int i, String s, String s1, String s2) {
		log.info("visitMethodInsn(" + i + ", '" + s + "', '" + s1 + "', '" + s2 + "')");
		methodVisitor.visitMethodInsn(i, s, s1, s2);
	}

	public void visitJumpInsn(int i, Label label) {
		log.info("visitJumpInsn(" + i + ", " + label + ")");
		methodVisitor.visitJumpInsn(i, label);
	}

	public void visitLabel(Label label) {
		log.info("visitLabel(" + label + ")");
		methodVisitor.visitLabel(label);
	}

	public void visitLdcInsn(Object o) {
		log.info("visitLdcInsn(" + o + ")");
		methodVisitor.visitLdcInsn(o);
	}

	public void visitIincInsn(int i, int i1) {
		log.info("visitIincInsn(" + i + ", " + i1 + ")");
		methodVisitor.visitIincInsn(i, i1);
	}

	public void visitTableSwitchInsn(int i, int i1, Label label, Label[] labels) {
		log.info("visitTableSwitchInsn(" + i + ", " + i1 + ", " + label + ", " + labels + ")");
		methodVisitor.visitTableSwitchInsn(i, i1, label, labels);
	}

	public void visitLookupSwitchInsn(Label label, int[] ints, Label[] labels) {
		log.info("visitLookupSwitchInsn(" + label + ", " + ints + ", " + labels + ")");
		methodVisitor.visitLookupSwitchInsn(label, ints, labels);
	}

	public void visitMultiANewArrayInsn(String s, int i) {
		log.info("visitMultiANewArrayInsn('" + s + "', " + i + ")");
		methodVisitor.visitMultiANewArrayInsn(s, i);
	}

	public void visitTryCatchBlock(Label label, Label label1, Label label2, String s) {
		log.info("visitTryCatchBlock(" + label + ", " + label1 + ", " + label2 + ", '" + s + "')");
		methodVisitor.visitTryCatchBlock(label, label1, label2, s);
	}

	public void visitLocalVariable(String s, String s1, String s2, Label label, Label label1, int i) {
		log.info("visitLocalVariable('" + s + "', '" + s1 + "', '" + s2 + "', " + label + ", " + label1 + ", " + i + ")");
		methodVisitor.visitLocalVariable(s, s1, s2, label, label1, i);
	}

	public void visitLineNumber(int i, Label label) {
		log.info("visitLineNumber(" + i + ", " + label + ")");
		methodVisitor.visitLineNumber(i, label);
	}

	public void visitMaxs(int i, int i1) {
		log.info("visitMaxs(" + i + ", " + i1 + ")");
		methodVisitor.visitMaxs(i, i1);
	}

	public void visitEnd() {
		log.info("visitEnd()");
		methodVisitor.visitEnd();
	}
}
