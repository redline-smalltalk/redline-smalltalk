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
}
