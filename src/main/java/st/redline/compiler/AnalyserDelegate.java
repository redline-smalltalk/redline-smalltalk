/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

interface AnalyserDelegate extends NodeVisitor {
	byte[] classBytes();
	void visitBegin(Program program);
	void visitEnd(Program program);
}
