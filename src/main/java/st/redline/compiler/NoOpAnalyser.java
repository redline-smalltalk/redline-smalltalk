/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class NoOpAnalyser implements AnalyserDelegate {

	private final Analyser analyser;

	NoOpAnalyser(Analyser analyser) {
		this.analyser = analyser;
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
	}

	public void visitEnd(Statements statements) {
	}

	public void visitBegin(AnswerStatement answerStatement) {
	}

	public void visitEnd(AnswerStatement answerStatement) {
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

	public boolean skipBlockVisit(Block block) {
		return true;
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

	public byte[] classBytes() {
		return new byte[0];
	}
}
