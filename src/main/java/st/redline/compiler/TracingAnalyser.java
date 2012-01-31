package st.redline.compiler;

public class TracingAnalyser implements AnalyserDelegate {

	private final AnalyserDelegate delegate;

	public TracingAnalyser(AnalyserDelegate delegate) {
		this.delegate = delegate;
	}

	void trace(String trace) {
		System.out.println(trace);
	}

	public byte[] classBytes() {
		trace("classBytes()");
		return delegate.classBytes();
	}

	public void visitBegin(Program program) {
		trace("visitBegin(Program) " + program);
		delegate.visitBegin(program);
	}

	public void visitEnd(Program program) {
		trace("visitEnd(Program) " + program);
		delegate.visitEnd(program);
	}

	public void visitBegin(Temporaries temporaries) {
		trace("visitBegin(Temporaries) " + temporaries);
		delegate.visitBegin(temporaries);
	}

	public void visitEnd(Temporaries temporaries) {
		trace("visitEnd(Temporaries) " + temporaries);
		delegate.visitEnd(temporaries);
	}

	public void visitBegin(Statements statements) {
		trace("visitBegin(Statements) " + statements);
		delegate.visitBegin(statements);
	}

	public void visitEnd(Statements statements) {
		trace("visitEnd(Statements) " + statements);
		delegate.visitEnd(statements);
	}

	public void visit(Temporary temporary, String value, int line) {
		trace("visit(Temporary) " + temporary + " " + String.valueOf(value) + " " + line);
		delegate.visit(temporary, value, line);
	}

	public void visit(PrimaryExpression primaryExpression, int line) {
		trace("visit(PrimaryExpression) " + primaryExpression + " " + line);
		delegate.visit(primaryExpression, line);
	}

	public void visit(PrimaryStatements primaryStatements, int line) {
		trace("visit(PrimaryStatements) " + primaryStatements + " " + line);
		delegate.visit(primaryStatements, line);
	}

	public void visitBegin(SimpleExpression simpleExpression) {
		trace("visitBegin(SimpleExpression) " + simpleExpression);
		delegate.visitBegin(simpleExpression);
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		trace("visitEnd(SimpleExpression) " + simpleExpression);
		delegate.visitEnd(simpleExpression);
	}

	public void visitBegin(Cascade cascade) {
		trace("visitBegin(Cacade) " + cascade);
		delegate.visitBegin(cascade);
	}

	public void visitEnd(Cascade cascade) {
		trace("visitEnd(Cacade) " + cascade);
		delegate.visitEnd(cascade);
	}

	public void visitBegin(UnaryExpression unaryExpression) {
		trace("visitBegin(UnaryExpression) " + unaryExpression);
		delegate.visitBegin(unaryExpression);
	}

	public void visitEnd(UnaryExpression unaryExpression) {
		trace("visitEnd(UnaryExpression) " + unaryExpression);
		delegate.visitEnd(unaryExpression);
	}

	public void visitBegin(BinaryExpression binaryExpression) {
		trace("visitBegin(BinaryExpression) " + binaryExpression);
		delegate.visitBegin(binaryExpression);
	}

	public void visitEnd(BinaryExpression binaryExpression) {
		trace("visitEnd(BinaryExpression) " + binaryExpression);
		delegate.visitEnd(binaryExpression);
	}

	public void visitBegin(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
		trace("visitBegin(KeywordExpression) " + keywordExpression + " " + String.valueOf(selector) + " " + argumentCount + " " + line);
		delegate.visitBegin(keywordExpression, selector, argumentCount, line);
	}

	public void visitEnd(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
		trace("visitEnd(KeywordExpression) " + keywordExpression + " " + String.valueOf(selector) + " " + argumentCount + " " + line);
		delegate.visitEnd(keywordExpression, selector, argumentCount, line);
	}

	public void visitBegin(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
		trace("visitBegin(KeywordMessageElement) " + keywordMessageElement + " " + String.valueOf(selector) + " " + argumentCount + " " + line);
		delegate.visitBegin(keywordMessageElement, selector, argumentCount, line);
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
		trace("visitEnd(KeywordMessageElement) " + keywordMessageElement + " " + String.valueOf(selector) + " " + argumentCount + " " + line);
		delegate.visitEnd(keywordMessageElement, selector, argumentCount, line);
	}

	public void visitBegin(AssignmentExpression assignmentExpression) {
		trace("visitBegin(AssignmentExpression) " + assignmentExpression);
		delegate.visitBegin(assignmentExpression);
	}

	public void visitEnd(AssignmentExpression assignmentExpression) {
		trace("visitEnd(AssignmentExpression) " + assignmentExpression);
		delegate.visitEnd(assignmentExpression);
	}

	public void visitBegin(Array array) {
		trace("visitBegin(Array) " + array);
		delegate.visitBegin(array);
	}

	public void visitEnd(Array array) {
		trace("visitEnd(Array) " + array);
		delegate.visitEnd(array);
	}

	public void visitBegin(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
		trace("visitBegin(BinarySelectorMessageElement) " + binarySelectorMessageElement + " " + String.valueOf(selector) + " " + line);
		delegate.visitBegin(binarySelectorMessageElement, selector, line);
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
		trace("visitEnd(BinarySelectorMessageElement) " + binarySelectorMessageElement + " " + String.valueOf(selector) + " " + line);
		delegate.visitEnd(binarySelectorMessageElement, selector, line);
	}

	public void visitBegin(Block block, int line) {
		trace("visitBegin(Block) " + block + " " + line);
		delegate.visitBegin(block, line);
	}

	public void visitEnd(Block block, int line) {
		trace("visitEnd(Block) " + block + " " + line);
		delegate.visitEnd(block, line);
	}

	public void visitBegin(BlockArguments blockArguments, int argumentCount) {
		trace("visitBegin(BlockArguments) " + blockArguments + " " + argumentCount);
		delegate.visitBegin(blockArguments, argumentCount);
	}

	public void visitEnd(BlockArguments blockArguments, int argumentCount) {
		trace("visitEnd(BlockArguments) " + blockArguments + " " + argumentCount);
		delegate.visitEnd(blockArguments, argumentCount);
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
		trace("visit(BinaryObjectDescription) " + binaryObjectDescription);
		delegate.visit(binaryObjectDescription);
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
		trace("visit(UnaryObjectDescription) " + unaryObjectDescription);
		delegate.visit(unaryObjectDescription);
	}

	public void visit(Identifier identifier, String value, int line) {
		trace("visit(Identifier) " + identifier + " " + String.valueOf(value) + " " + line);
		delegate.visit(identifier, value, line);
	}

	public void visit(Number number, String value, int line) {
		trace("visit(Number) " + number + " " + String.valueOf(value) + " " + line);
		delegate.visit(number, value, line);
	}

	public void visit(BlockArgument blockArgument, String value, int line) {
		trace("visit(BlockArgument) " + blockArgument + " " + String.valueOf(value) + " " + line);
		delegate.visit(blockArgument, value, line);
	}

	public void visit(Self self, int line) {
		trace("visit(Self) " + self + " " + line);
		delegate.visit(self, line);
	}

	public void visit(Super aSuper, int line) {
		trace("visit(Super) " + aSuper + " " + line);
		delegate.visit(aSuper, line);
	}

	public void visit(True aTrue, int line) {
		trace("visit(True) " + aTrue + " " + line);
		delegate.visit(aTrue, line);
	}

	public void visit(False aFalse, int line) {
		trace("visit(False) " + aFalse + " " + line);
		delegate.visit(aFalse, line);
	}

	public void visit(Nil nil, int line) {
		trace("visit(Nil) " + nil + " " + line);
		delegate.visit(nil, line);
	}

	public void visit(JVM jvm, int line) {
		trace("visit(JVM) " + jvm + " " + line);
		delegate.visit(jvm, line);
	}

	public void visit(ArrayConstant arrayConstant, int line) {
		trace("visit(ArrayConstant) " + arrayConstant + " " + line);
		delegate.visit(arrayConstant, line);
	}

	public void visit(UnarySelector unarySelector, String selector, int line) {
		trace("visit(UnarySelector) " + unarySelector + " " + String.valueOf(selector) + " " + line);
		delegate.visit(unarySelector, selector, line);
	}

	public void visit(BinarySelector binarySelector, String selector, int line) {
		trace("visit(BinarySelector) " + binarySelector + " " + String.valueOf(selector) + " " + line);
		delegate.visit(binarySelector, selector, line);
	}

	public void visit(CharacterConstant characterConstant, String value, int index, int line) {
		trace("visit(CharacterConstant) " + characterConstant + " " + String.valueOf(value) + " " + index + " " + line);
		delegate.visit(characterConstant, value, index, line);
	}

	public void visit(StringConstant stringConstant, String value, int index, int line) {
		trace("visit(StringConstant) " + stringConstant + " " + String.valueOf(value) + " " + index + " " + line);
		delegate.visit(stringConstant, value, index, line);
	}

	public void visit(Symbol symbol, String value, int index, int line) {
		trace("visit(Symbol) " + symbol + " " + String.valueOf(value) + " " + index + " " + line);
		delegate.visit(symbol, value, index, line);
	}

	public void visit(SymbolConstant symbolConstant, String value, int line) {
		trace("visit(SymbolConstant) " + symbolConstant + " " + String.valueOf(value) + " " + line);
		delegate.visit(symbolConstant, value, line);
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		trace("visit(UnarySelectorMessageElement) " + unarySelectorMessageElement + " " + String.valueOf(value) + " " + line);
		delegate.visit(unarySelectorMessageElement, value, line);
	}

	public void visit(Primitive primitive, String keyword, int line, String digits) {
		trace("visit(Primitive) " + primitive + " " + String.valueOf(keyword) + " " + line + " " + digits);
		delegate.visit(primitive, keyword, line, digits);
	}
}
