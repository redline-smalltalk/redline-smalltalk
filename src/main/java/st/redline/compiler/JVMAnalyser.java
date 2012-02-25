/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class JVMAnalyser implements AnalyserDelegate {

	protected final Analyser analyser;
	protected final ClassBytecodeWriter writer;
	private final boolean verbose;

	private int nesting;

	JVMAnalyser(Analyser analyser, ClassBytecodeWriter classBytecodeWriter, boolean verbose) {
		this.analyser = analyser;
		this.writer = classBytecodeWriter;
		this.verbose = verbose;
		this.nesting = 1;
	}

	public byte[] classBytes() {
		throw new IllegalStateException("Should not be calling this on JVMAnalyser.");
	}

	public void visitBegin(Program program) {
	}

	public void visitEnd(Program program) {
	}

	public void visitBegin(Temporaries temporaries) {
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visitBegin(Statements statements) {
		nesting++;
	}

	public void visitEnd(Statements statements) {
		nesting--;
		if (nesting == 0)
			analyser.previousDelegate();
	}

	public void visitBegin(SimpleExpression simpleExpression) {
	}

	public void visitEnd(SimpleExpression simpleExpression) {
	}

	public void visitBegin(Cascade cascade) {
	}

	public void visitEnd(Cascade cascade) {
	}

	public void visitBegin(UnaryExpression unaryExpression) {
	}

	public void visitEnd(UnaryExpression unaryExpression) {
	}

	public void visitBegin(BinaryExpression binaryExpression) {
	}

	public void visitEnd(BinaryExpression binaryExpression) {
	}

	public void visitBegin(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
	}

	public void visitEnd(KeywordExpression keywordExpression, String selector, int argumentCount, int line) {
	}

	public void visitBegin(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String selector, int argumentCount, int line) {
	}

	public void visitBegin(AssignmentExpression assignmentExpression) {
	}

	public void visitEnd(AssignmentExpression assignmentExpression) {
	}

	public void visitBegin(Array array) {
	}

	public void visitEnd(Array array) {
	}

	public void visitBegin(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String selector, int line) {
	}

	public void visitBegin(Block block, int line) {
	}

	public void visitEnd(Block block, int line) {
	}

	public void visitBegin(BlockArguments blockArguments, int argumentCount) {
	}

	public void visitEnd(BlockArguments blockArguments, int argumentCount) {
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(Temporary temporary, String value, int line) {
	}

	public void visit(PrimaryExpression primaryExpression, int line) {
	}

	public void visit(PrimaryStatements primaryStatements, int line) {
	}

	public void visit(Identifier identifier, String value, int line) {
	}

	public void visit(Number number, String value, int index, boolean insideArray, int line) {
	}

	public void visit(BlockArgument blockArgument, String value, int line) {
	}

	public void visit(Self self, int line) {
	}

	public void visit(Super aSuper, int line) {
	}

	public void visit(True aTrue, int line) {
	}

	public void visit(False aFalse, int line) {
	}

	public void visit(Nil nil, int line) {
	}

	public void visit(JVM jvm, int line) {
		writer.visitLine(line);
	}

	public void visit(ArrayConstant arrayConstant, int line) {
	}

	public void visit(UnarySelector unarySelector, String selector, int line) {
	}

	public void visit(BinarySelector binarySelector, String selector, int line) {
	}

	public void visit(CharacterConstant characterConstant, String value, int index, boolean insideArray, int line) {
	}

	public void visit(StringConstant stringConstant, String value, int index, boolean insideArray, int line) {
	}

	public void visit(Symbol symbol, String value, int index, boolean insideArray, int line) {
	}

	public void visit(SymbolConstant symbolConstant, String value, int line) {
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
	}

	public void visit(Primitive primitive, String keyword, int line, String digits) {
	}

	public boolean skipBlockVisit(Block block) {
		throw new IllegalStateException("Should not be calling this on JVMAnalyser.");
	}
}
