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

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
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