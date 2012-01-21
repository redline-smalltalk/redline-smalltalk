package st.redline.compiler;

import java.util.Stack;

public class Analyser implements NodeVisitor {

	private final Stack<AnalyserDelegate> delegates;
	private AnalyserDelegate delegate;

	public Analyser(String className, String packageName, boolean verbose) {
		delegates = new Stack<AnalyserDelegate>();
		currentDelegate(new ProgramAnalyser(className, packageName, verbose));
	}

	void currentDelegate(AnalyserDelegate analyserDelegate) {
		delegate = analyserDelegate;
		delegates.push(delegate);
	}

	AnalyserDelegate currentDelegate() {
		return delegates.peek();
	}

	public byte[] classBytes() {
		return delegate.classBytes();
	}

	public void visitBegin(Program program) {
		delegate.visitBegin(program);
	}

	public void visitEnd(Program program) {
		delegate.visitEnd(program);
	}

	public void visitBegin(Temporaries temporaries) {
		delegate.visitBegin(temporaries);
	}

	public void visitEnd(Temporaries temporaries) {
		delegate.visitEnd(temporaries);
	}

	public void visitBegin(Statements statements) {
		delegate.visitBegin(statements);
	}

	public void visitEnd(Statements statements) {
		delegate.visitEnd(statements);
	}

	public void visit(Temporary temporary, String value, int line) {
		delegate.visit(temporary, value, line);
	}

	public void visitBegin(SimpleExpression simpleExpression) {
		delegate.visitBegin(simpleExpression);
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		delegate.visitEnd(simpleExpression);
	}

	public void visitBegin(Cascade cascade) {
		delegate.visitBegin(cascade);
	}

	public void visitEnd(Cascade cascade) {
		delegate.visitEnd(cascade);
	}

	public void visit(Identifier identifier, String value, int line) {
		delegate.visit(identifier, value, line);
	}

	public void visit(Self self, int line) {
		delegate.visit(self, line);
	}

	public void visit(Super aSuper, int line) {
		delegate.visit(aSuper, line);
	}

	public void visit(True aTrue, int line) {
		delegate.visit(aTrue, line);
	}

	public void visit(False aFalse, int line) {
		delegate.visit(aFalse, line);
	}

	public void visit(Nil nil, int line) {
		delegate.visit(nil, line);
	}

	public void visit(JVM jvm, int line) {
		delegate.visit(jvm, line);
	}
}
