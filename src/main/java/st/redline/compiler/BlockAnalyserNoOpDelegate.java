/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.List;
import java.util.Stack;

public class BlockAnalyserNoOpDelegate extends BlockAnalyserDelegate {

	public BlockAnalyserNoOpDelegate(BlockAnalyser blockAnalyser, String className, String packageName, int countOfArguments, boolean isClassMethod, Analyser containingAnalyser, Block thisBlock) {
		super(blockAnalyser, className, packageName, countOfArguments, isClassMethod, containingAnalyser, thisBlock);
	}

	public int methodArgumentCount() {
		return 0;
	}

	public byte[] classBytes() {
		return null;
	}

	public boolean continueBlockVisit() {
		return true;
	}

	public void visit(Program program) {
	}

	public void visitEnd(Program program) {
	}

	public void visit(Temporaries temporaries) {
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visit(Temporary temporary, int index, String value, int line) {
	}

	public void visit(VariableName variableName, String value, int line) {
	}

	protected Temporary temporaryAt(String name) {
		return null;
	}

	protected boolean isTemporary(String name) {
		return false;
	}

	protected boolean isLocalTemporary(String name) {
		return false;
	}

	protected boolean isOuterContextTemporary(String name) {
		return false;
	}

	protected boolean isOuterContextMethodArgument(String name) {
		return false;
	}

	protected VariableName outerContextMethodArgument(String name) {
		return null;
	}

	public void visit(Statements statements) {
	}

	public void visitEnd(Statements statements) {
	}

	public void visit(AnswerExpression answerExpression) {
	}

	public void visit(Methods methods) {
	}

	public void visitEnd(Methods methods) {
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
	}

	public void visit(Keyword keyword, String value, int line) {
	}

	public void visit(AssignmentExpression assignmentExpression) {
	}

	public void visit(SimpleExpression simpleExpression) {
	}

	public void visitEnd(SimpleExpression simpleExpression) {
	}

	public void visit(Cascade cascade) {
	}

	public void visitEnd(Cascade cascade) {
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, int argumentCount) {
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String keyword, int line, int argumentCount) {
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
	}

	public void visit(UnaryExpression unaryExpression) {
	}

	public void visitEnd(UnaryExpression unaryExpression) {
	}

	public void visit(BinaryExpression binaryExpression) {
	}

	public void visitEnd(BinaryExpression binaryExpression) {
	}

	public void visit(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
	}

	public void visitEnd(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
	}

	public void visit(PrimaryExpression primaryExpression) {
	}

	public void visit(PrimaryStatements primaryStatements) {
	}

	public void visit(Primitive primitive, String value, int line) {
	}

	public void visit(Symbol symbol, String value, int line) {
	}

	public void visit(Array array) {
	}

	public void visitEnd(Array array) {
	}

	public void visit(Identifier identifier, String value, int line) {
	}

	public void visit(LiteralSymbol literalSymbol, String value, int line) {
	}

	public void visit(LiteralArray literalArray) {
	}

	public void visitEnd(LiteralArray literalArray) {
	}

	public void visit(ArrayConstantElement arrayConstantElement) {
	}

	public void visit(CharacterConstant characterConstant, String value, int line) {
	}

	public void visit(StringConstant stringConstant, String value, int line) {
	}

	public void visit(StringChunk stringChunk, String value, int line) {
	}

	public void visit(LiteralString literalString, String value, int line) {
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
	}

	public void visit(Block block) {
	}

	protected void blockSequence(int blockSequence) {
	}

	protected int blockSequence() {
		return 0;
	}

	public void visitEnd(Block block) {
//		System.out.println("BlockAnalyserNoOp.Block() Analysis end " + block + " " + thisBlock);
		blockAnalyser.useDelegate(block);
	}

	public void visit(SelfReservedWord selfReservedWord, int line) {
	}

	public void visit(SuperReservedWord superReservedWord, int line) {
	}

	public void visit(TrueReservedWord selfReservedWord, int line) {
	}

	public void visit(FalseReservedWord selfReservedWord, int line) {
	}

	public void visit(NilReservedWord selfReservedWord, int line) {
	}

	public void visit(SmalltalkReservedWord smalltalkReservedWord, int line) {
	}

	public void visit(JvmInsn jvmInsn, int opcode, int line) {
	}

	public void visit(JvmLdcInsn jvmLdcInsn, String value, int line) {
	}

	public void visit(JvmVarInsn jvmVarInsn, int opcode, int number, int line) {
	}

	public void visit(JvmMethodInsn jvmMethodInsn, int opcode, String owner, String name, String description, int line) {
	}

	public void visit(JvmFieldInsn jvmFieldInsn, int opcode, String owner, String name, String description, int line) {
	}

	public void visit(JvmTypeInsn jvmTypeInsn, int opcode, String type, int line) {
	}

	public void visit(JvmLoadJavaValue jvmLoadJavaValue, int line) {
	}

	public void visit(JvmStoreJavaValue jvmStoreJavaValue, int line) {
	}

	public void visit(JvmIincInsn jvmIincInsn, int variable, int increment, int line) {
	}

	public void visit(JvmIntInsn jvmIntInsn, int opcode, int operand, int line) {
	}

	protected void initialize() {
	}

	public int methodTemporariesCount() {
		return 0;
	}

	public boolean continueMethodVisit() {
		return false;
	}

	protected boolean isMethodArgument(String value) {
		return false;
	}

	protected boolean isLocalMethodArgument(String value) {
		return false;
	}

	protected void loadMethodArgument(int line, String value, boolean isLocal) {
	}

	protected VariableName methodArgument(String name) {
		return null;
	}

	public void visit(BlockVariableName blockVariableName, String value, int line) {
	}

	public void visit(InstanceMethod instanceMethod) {
	}

	public void visitEnd(InstanceMethod instanceMethod) {
	}

	public void visit(ClassMethod classMethod) {
	}

	public void visitEnd(ClassMethod classMethod) {
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
	}

	protected void registerMethodArgument(VariableName variableName) {
	}
}
