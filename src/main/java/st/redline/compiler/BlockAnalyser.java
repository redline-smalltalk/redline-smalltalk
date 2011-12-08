/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.List;
import java.util.Stack;

public class BlockAnalyser implements NodeVisitor {

	private final Stack<BlockAnalyserDelegate> blockAnalysers;

	public BlockAnalyser(String className, String packageName, int countOfArguments, boolean isClassMethod, Analyser containingAnalyser, Block thisBlock) {
		blockAnalysers = new Stack<BlockAnalyserDelegate>();
		blockAnalysers.push(new BlockAnalyserDelegate(this, className, packageName, countOfArguments, isClassMethod, containingAnalyser, thisBlock));
	}

	private BlockAnalyserDelegate blockAnalyser() {
		return blockAnalysers.peek();
	}

	public int methodArgumentCount() {
		return blockAnalyser().methodArgumentCount();
	}

	public byte[] classBytes() {
		return blockAnalyser().classBytes();
	}

	public boolean continueBlockVisit() {
		return blockAnalyser().continueBlockVisit();
	}

	public void visit(Program program) {
		blockAnalyser().visit(program);
	}

	public void visitEnd(Program program) {
		blockAnalyser().visitEnd(program);
	}

	public void visit(Temporaries temporaries) {
		blockAnalyser().visit(temporaries);
	}

	public void visitEnd(Temporaries temporaries) {
		blockAnalyser().visitEnd(temporaries);
	}

	public void visit(Temporary temporary, int index, String value, int line) {
		blockAnalyser().visit(temporary, index, value, line);
	}

	public void visit(VariableName variableName, String value, int line) {
		blockAnalyser().visit(variableName, value, line);
	}

	protected Temporary temporaryAt(String name) {
		return blockAnalyser().temporaryAt(name);
	}

	protected boolean isTemporary(String name) {
		return blockAnalyser().isTemporary(name);
	}

	protected boolean isLocalTemporary(String name) {
		return blockAnalyser().isLocalTemporary(name);
	}

	protected boolean isOuterContextTemporary(String name) {
		return blockAnalyser().isOuterContextTemporary(name);
	}

	protected boolean isOuterContextMethodArgument(String name) {
		return blockAnalyser().isOuterContextMethodArgument(name);
	}

	protected VariableName outerContextMethodArgument(String name) {
		return blockAnalyser().outerContextMethodArgument(name);
	}

	public void visit(Statements statements) {
		blockAnalyser().visit(statements);
	}

	public void visitEnd(Statements statements) {
		blockAnalyser().visitEnd(statements);
	}

	public void visit(AnswerExpression answerExpression) {
		blockAnalyser().visit(answerExpression);
	}

	public void visit(Methods methods) {
		blockAnalyser().visit(methods);
	}

	public void visitEnd(Methods methods) {
		blockAnalyser().visitEnd(methods);
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
		blockAnalyser().visit(unarySelector, value, line);
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
		blockAnalyser().visit(binarySelector, value, line);
	}

	public void visit(Keyword keyword, String value, int line) {
		blockAnalyser().visit(keyword, value, line);
	}

	public void visit(AssignmentExpression assignmentExpression) {
		blockAnalyser().visit(assignmentExpression);
	}

	public void visit(SimpleExpression simpleExpression) {
		blockAnalyser().visit(simpleExpression);
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		blockAnalyser().visitEnd(simpleExpression);
	}

	public void visit(Cascade cascade) {
		blockAnalyser().visit(cascade);
	}

	public void visitEnd(Cascade cascade) {
		blockAnalyser().visitEnd(cascade);
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		blockAnalyser().visit(unarySelectorMessageElement, value, line);
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
		blockAnalyser().visit(binarySelectorMessageElement, value, line, unaryObjectDescription);
	}

	public void visitEnd(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
		blockAnalyser().visitEnd(binarySelectorMessageElement, value, line, unaryObjectDescription);
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, int argumentCount) {
		blockAnalyser().visit(keywordMessageElement, keyword, line, argumentCount);
	}

	public void visitEnd(KeywordMessageElement keywordMessageElement, String keyword, int line, int argumentCount) {
		blockAnalyser().visitEnd(keywordMessageElement, keyword, line, argumentCount);
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
		blockAnalyser().visit(unaryObjectDescription);
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
		blockAnalyser().visit(binaryObjectDescription);
	}

	public void visit(UnaryExpression unaryExpression) {
		blockAnalyser().visit(unaryExpression);
	}

	public void visitEnd(UnaryExpression unaryExpression) {
		blockAnalyser().visitEnd(unaryExpression);
	}

	public void visit(BinaryExpression binaryExpression) {
		blockAnalyser().visit(binaryExpression);
	}

	public void visitEnd(BinaryExpression binaryExpression) {
		blockAnalyser().visitEnd(binaryExpression);
	}

	public void visit(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		blockAnalyser().visit(keywordExpression, keywords, argumentCount, line);
	}

	public void visitEnd(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		blockAnalyser().visitEnd(keywordExpression, keywords, argumentCount, line);
	}

	public void visit(PrimaryExpression primaryExpression) {
		blockAnalyser().visit(primaryExpression);
	}

	public void visit(PrimaryStatements primaryStatements) {
		blockAnalyser().visit(primaryStatements);
	}

	public void visit(Primitive primitive, String value, int line) {
		blockAnalyser().visit(primitive, value, line);
	}

	public void visit(Symbol symbol, String value, int line) {
		blockAnalyser().visit(symbol, value, line);
	}

	public void visit(Array array) {
		blockAnalyser().visit(array);
	}

	public void visitEnd(Array array) {
		blockAnalyser().visitEnd(array);
	}

	public void visit(Identifier identifier, String value, int line) {
		blockAnalyser().visit(identifier, value, line);
	}

	public void visit(LiteralSymbol literalSymbol, String value, int line) {
		blockAnalyser().visit(literalSymbol, value, line);
	}

	public void visit(LiteralArray literalArray) {
		blockAnalyser().visit(literalArray);
	}

	public void visitEnd(LiteralArray literalArray) {
		blockAnalyser().visitEnd(literalArray);
	}

	public void visit(ArrayConstantElement arrayConstantElement) {
		blockAnalyser().visit(arrayConstantElement);
	}

	public void visit(CharacterConstant characterConstant, String value, int line) {
		blockAnalyser().visit(characterConstant, value, line);
	}

	public void visit(StringConstant stringConstant, String value, int line) {
		blockAnalyser().visit(stringConstant, value, line);
	}

	public void visit(StringChunk stringChunk, String value, int line) {
		blockAnalyser().visit(stringChunk, value, line);
	}

	public void visit(LiteralString literalString, String value, int line) {
		blockAnalyser().visit(literalString, value, line);
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
		blockAnalyser().visit(literalCharacter, value, line);
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
		blockAnalyser().visit(numberConstant, value, line);
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
		blockAnalyser().visit(literalNumber, value, line);
	}

	public void visit(Block block) {
		blockAnalyser().visit(block);
	}

	protected void blockSequence(int blockSequence) {
		blockAnalyser().blockSequence(blockSequence);
	}

	protected int blockSequence() {
		return blockAnalyser().blockSequence();
	}

	public void visitEnd(Block block) {
		blockAnalyser().visitEnd(block);
	}

	public void visit(SelfReservedWord selfReservedWord, int line) {
		blockAnalyser().visit(selfReservedWord, line);
	}

	public void visit(SuperReservedWord superReservedWord, int line) {
		blockAnalyser().visit(superReservedWord, line);
	}

	public void visit(TrueReservedWord selfReservedWord, int line) {
		blockAnalyser().visit(selfReservedWord, line);
	}

	public void visit(FalseReservedWord selfReservedWord, int line) {
		blockAnalyser().visit(selfReservedWord, line);
	}

	public void visit(NilReservedWord selfReservedWord, int line) {
		blockAnalyser().visit(selfReservedWord, line);
	}

	public void visit(SmalltalkReservedWord smalltalkReservedWord, int line) {
		blockAnalyser().visit(smalltalkReservedWord, line);
	}

	public void visit(JvmInsn jvmInsn, int opcode, int line) {
		blockAnalyser().visit(jvmInsn, opcode, line);
	}

	public void visit(JvmLdcInsn jvmLdcInsn, String value, int line) {
		blockAnalyser().visit(jvmLdcInsn, value, line);
	}

	public void visit(JvmVarInsn jvmVarInsn, int opcode, int number, int line) {
		blockAnalyser().visit(jvmVarInsn, opcode, number, line);
	}

	public void visit(JvmMethodInsn jvmMethodInsn, int opcode, String owner, String name, String description, int line) {
		blockAnalyser().visit(jvmMethodInsn, opcode, owner, name, description, line);
	}

	public void visit(JvmFieldInsn jvmFieldInsn, int opcode, String owner, String name, String description, int line) {
		blockAnalyser().visit(jvmFieldInsn, opcode, owner, name, description, line);
	}

	public void visit(JvmTypeInsn jvmTypeInsn, int opcode, String type, int line) {
		blockAnalyser().visit(jvmTypeInsn, opcode, type, line);
	}

	public void visit(JvmLoadJavaValue jvmLoadJavaValue, int line) {
		blockAnalyser().visit(jvmLoadJavaValue, line);
	}

	public void visit(JvmStoreJavaValue jvmStoreJavaValue, int line) {
		blockAnalyser().visit(jvmStoreJavaValue, line);
	}

	public void visit(JvmIincInsn jvmIincInsn, int variable, int increment, int line) {
		blockAnalyser().visit(jvmIincInsn, variable, increment, line);
	}

	public void visit(JvmIntInsn jvmIntInsn, int opcode, int operand, int line) {
		blockAnalyser().visit(jvmIntInsn, opcode, operand, line);
	}

	protected void initialize() {
		blockAnalyser().initialize();
	}

	public int methodTemporariesCount() {
		return blockAnalyser().methodTemporariesCount();
	}

	public boolean continueMethodVisit() {
		return blockAnalyser().continueMethodVisit();
	}

	protected boolean isMethodArgument(String value) {
		return blockAnalyser().isMethodArgument(value);
	}

	protected boolean isLocalMethodArgument(String value) {
		return blockAnalyser().isLocalMethodArgument(value);
	}

	protected void loadMethodArgument(int line, String value, boolean isLocal) {
		blockAnalyser().loadMethodArgument(line, value, isLocal);
	}

	protected VariableName methodArgument(String name) {
		return blockAnalyser().methodArgument(name);
	}

	public void visit(BlockVariableName blockVariableName, String value, int line) {
		blockAnalyser().visit(blockVariableName, value, line);
	}

	public void visit(InstanceMethod instanceMethod) {
		blockAnalyser().visit(instanceMethod);
	}

	public void visitEnd(InstanceMethod instanceMethod) {
		blockAnalyser().visitEnd(instanceMethod);
	}

	public void visit(ClassMethod classMethod) {
		blockAnalyser().visit(classMethod);
	}

	public void visitEnd(ClassMethod classMethod) {
		blockAnalyser().visitEnd(classMethod);
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
		blockAnalyser().visit(unarySelectorMessagePattern, value, line);
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
		blockAnalyser().visit(binarySelectorMessagePattern, binarySelector, binarySelectorLine, variableName);
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
		blockAnalyser().visit(keywordMessagePattern, keywords, keywordLine, variableNames);
	}

	protected void registerMethodArgument(VariableName variableName) {
		blockAnalyser().registerMethodArgument(variableName);
	}
}
