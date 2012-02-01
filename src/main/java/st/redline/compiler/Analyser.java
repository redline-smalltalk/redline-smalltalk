package st.redline.compiler;

import java.util.Stack;

public class Analyser implements NodeVisitor {

	private final Stack<AnalyserDelegate> delegates;
	private AnalyserDelegate delegate;

	public Analyser(String className, String packageName, boolean verbose) {
		delegates = new Stack<AnalyserDelegate>();
		currentDelegate(verbose ? verboseDelegate(className, packageName)
								: normalDelegate(className, packageName, verbose));
	}

	AnalyserDelegate normalDelegate(String className, String packageName, boolean verbose) {
		return new ProgramAnalyser(className, packageName, verbose);
	}

	AnalyserDelegate verboseDelegate(String className, String packageName) {
		return new TracingAnalyser(normalDelegate(className, packageName, true));
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

	public void visit(PrimaryExpression primaryExpression, int line) {
		delegate.visit(primaryExpression, line);
	}

	public void visit(PrimaryStatements primaryStatements, int line) {
		delegate.visit(primaryStatements, line);
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

	public void visitBegin(UnaryExpression unaryExpression) {
		delegate.visitBegin(unaryExpression);
	}

	public void visitEnd(UnaryExpression unaryExpression) {
		delegate.visitEnd(unaryExpression);
	}

	public void visitBegin(BinaryExpression binaryExpression) {
		delegate.visitBegin(binaryExpression);
	}

	public void visitEnd(BinaryExpression binaryExpression) {
		delegate.visitEnd(binaryExpression);
	}

	public void visitBegin(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
		delegate.visitBegin(keywordExpression, selector, argumentCount, line);
	}

	public void visitEnd(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
		delegate.visitEnd(keywordExpression, selector, argumentCount, line);
	}

	public void visitBegin(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
		delegate.visitBegin(keywordMessageElement, selector, argumentCount, line);
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
		delegate.visitEnd(keywordMessageElement, selector, argumentCount, line);
	}

	public void visitBegin(AssignmentExpression assignmentExpression) {
		delegate.visitBegin(assignmentExpression);
	}

	public void visitEnd(AssignmentExpression assignmentExpression) {
		delegate.visitEnd(assignmentExpression);
	}

	public void visitBegin(Array array) {
		delegate.visitBegin(array);
	}

	public void visitEnd(Array array) {
		delegate.visitEnd(array);
	}

	public void visitBegin(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
		delegate.visitBegin(binarySelectorMessageElement, selector, line);
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
		delegate.visitEnd(binarySelectorMessageElement, selector, line);
	}

	public void visitBegin(Block block, int line) {
		delegate.visitBegin(block, line);
	}

	public void visitEnd(Block block, int line) {
		delegate.visitEnd(block, line);
	}

	public void visitBegin(BlockArguments blockArguments, int argumentCount) {
		delegate.visitBegin(blockArguments, argumentCount);
	}

	public void visitEnd(BlockArguments blockArguments, int argumentCount) {
		delegate.visitEnd(blockArguments, argumentCount);
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
		delegate.visit(binaryObjectDescription);
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
		delegate.visit(unaryObjectDescription);
	}

	public void visit(Identifier identifier, String value, int line) {
		delegate.visit(identifier, value, line);
	}

	public void visit(Number number, String value, int index, boolean insideArray, int line) {
		delegate.visit(number, value, index, insideArray, line);
	}

	public void visit(BlockArgument blockArgument, String value, int line) {
		delegate.visit(blockArgument, value, line);
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

	public void visit(ArrayConstant arrayConstant, int line) {
		delegate.visit(arrayConstant, line);
	}

	public void visit(UnarySelector unarySelector, String selector, int line) {
		delegate.visit(unarySelector, selector, line);
	}

	public void visit(BinarySelector binarySelector, String selector, int line) {
		delegate.visit(binarySelector, selector, line);
	}

	public void visit(CharacterConstant characterConstant, String value, int index, boolean insideArray, int line) {
		delegate.visit(characterConstant, value, index, insideArray, line);
	}

	public void visit(StringConstant stringConstant, String value, int index, boolean insideArray, int line) {
		delegate.visit(stringConstant, value, index, insideArray, line);
	}

	public void visit(Symbol symbol, String value, int index, boolean insideArray, int line) {
		delegate.visit(symbol, value, index, insideArray, line);
	}

	public void visit(SymbolConstant symbolConstant, String value, int line) {
		delegate.visit(symbolConstant, value, line);
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		delegate.visit(unarySelectorMessageElement, value, line);
	}

	public void visit(Primitive primitive, String keyword, int line, String digits) {
		delegate.visit(primitive, keyword, line, digits);
	}
}
