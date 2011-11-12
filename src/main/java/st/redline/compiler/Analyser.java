/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import st.redline.Primitives;
import st.redline.ProtoObject;

public class Analyser implements NodeVisitor {

	public static final Primitives primitives = new Primitives();

	protected final String className;
	protected final String packageName;

	protected ClassBytecodeWriter classBytecodeWriter;
	private boolean sendToSuper = false;
	private AbstractMethod currentMethod;
	protected int countOfArguments;
	protected boolean isClassMethod = false;
	private Map<String, Temporary> temporariesRegistry;
	private int arrayDepth;
	private int blockDepth;
	private int blockSequence;

	public Analyser(String className, String packageName) {
		this(className, packageName, 0, false);
	}

	public Analyser(String className, String packageName, int countOfArguments, boolean isClassMethod) {
		this.className = className;
		this.packageName = packageName;
		this.countOfArguments = countOfArguments;
		this.isClassMethod = isClassMethod;
		initialize();
	}

	protected void initialize() {
		classBytecodeWriter = new ClassBytecodeWriter(className, packageName);
	}

	public int methodArgumentCount() {
		return countOfArguments;
	}

	public byte[] classBytes() {
		return classBytecodeWriter.contents();
	}

	public boolean continueMethodVisit() {
		return false;
	}

	public boolean continueBlockVisit() {
		return false;
	}

	public int methodTemporariesCount() {
		throw new IllegalStateException("TODO - handle");
	}

	public void visit(Program program) {
		classBytecodeWriter.openClass();
	}

	public void visitEnd(Program program) {
		classBytecodeWriter.closeClass();
	}

	public void visit(Temporaries temporaries) {
		temporariesRegistry = new HashMap<String, Temporary>();
		classBytecodeWriter.callPrimitiveInitializeTemporaries(temporaries.line(), temporaries.size());
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visit(Temporary temporary, int index, String value, int line) {
		System.out.println("visitTemporary() " + value);
		temporariesRegistry.put(value, temporary);
	}

	public void visit(BlockVariableName blockVariableName, String value, int line) {
		throw new IllegalStateException("BlockAnalyser should be handling this.");
	}

	public void visit(VariableName variableName, String value, int line) {
		if (isTemporary(value)) {
			Temporary temporary = temporariesRegistry.get(value);
			if (variableName.isOnLoadSideOfExpression())
				classBytecodeWriter.callPrimitiveTemporaryAt(value, line, temporary.index);
			else
				classBytecodeWriter.callPrimitiveTemporaryPutAt(value, line, temporary.index);
		} else {
			if (variableName.isOnLoadSideOfExpression()) {
				if (isMethodArgument(value)) {
					loadMethodArgument(value);
				} else {
					classBytecodeWriter.callPrimitiveVariableAt(value, line, isClassMethod);
				}
			} else {
				if (isMethodArgument(value)) {
					throw new IllegalStateException("You can't store into method argument '" + value + "'.");
				} else {
					classBytecodeWriter.callPrimitiveVariablePutAt(value, line, isClassMethod);
				}
			}
		}
	}

	protected void loadMethodArgument(String value) {
		throw new IllegalStateException("Subclass should implement.");
	}

	protected boolean isMethodArgument(String value) {
		return false;
	}

	private boolean isTemporary(String name) {
		return temporariesRegistry != null && temporariesRegistry.containsKey(name);
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

	public void visit(InstanceMethod instanceMethod) {
		classBytecodeWriter.callPrimitiveVariableAt(instanceMethod.objectName, instanceMethod.line(), true);
		currentMethod = instanceMethod;
		isClassMethod = false;
	}

	public void visitEnd(InstanceMethod instanceMethod) {
		currentMethod = null;
	}

	public void visit(ClassMethod classMethod) {
		classBytecodeWriter.callPrimitiveVariableAt(classMethod.objectName, classMethod.line(), true);
		classBytecodeWriter.callClass();
		currentMethod = classMethod;
		isClassMethod = true;
	}

	public void visitEnd(ClassMethod classMethod) {
		currentMethod = null;
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
		String fullMethodName = createFullMethodName(value);
		primitives.registerMethodToBeCompiledAs(currentMethod, fullMethodName);
		classBytecodeWriter.callPrimitiveCompileMethod(fullMethodName, value, className, packageName, 0, isClassMethod);
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
		String fullMethodName = createFullMethodName(binarySelector);
		primitives.registerMethodToBeCompiledAs(currentMethod, fullMethodName);
		classBytecodeWriter.callPrimitiveCompileMethod(fullMethodName, binarySelector, className, packageName, 1, isClassMethod);
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
		String fullMethodName = createFullMethodName(keywords);
		primitives.registerMethodToBeCompiledAs(currentMethod, fullMethodName);
		classBytecodeWriter.callPrimitiveCompileMethod(fullMethodName, keywords, className, packageName, variableNames.size(), isClassMethod);
	}

	private String createFullMethodName(String name) {
		return packageName + File.separator + className + "$" + name;
	}

	public void visit(UnarySelector unarySelector, String value, int line) {
		classBytecodeWriter.unarySend(value, line, sendToSuper);
		sendToSuper = false;
	}

	public void visit(BinarySelector binarySelector, String value, int line) {
		classBytecodeWriter.binarySend(value, line, sendToSuper);
		sendToSuper = false;
	}

	public void visit(Keyword keyword, String value, int line) {
		// System.out.println("TODO Keyword() " + value);
	}

	public void visit(AssignmentExpression assignmentExpression) {
//		System.out.println("TODO AssignmentExpression() " + assignmentExpression);
	}

	public void visit(SimpleExpression simpleExpression) {
		sendToSuper = false;
	}

	public void visitEnd(SimpleExpression simpleExpression) {
		if (simpleExpression.isResultDuplicatedOnStack())
			classBytecodeWriter.stackDuplicate();
		if (!simpleExpression.isResultLeftOnStack())
			classBytecodeWriter.stackPop();
	}

	public void visit(Cascade cascade) {
		classBytecodeWriter.stackDuplicate();
	}

	public void visitEnd(Cascade cascade) {
		classBytecodeWriter.stackPop();
	}

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
		classBytecodeWriter.unarySend(value, line, sendToSuper);
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
		// System.out.println("TODO BinarySelectorMessageElement() " + value + " " + unaryObjectDescription);
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions) {
		// System.out.println("TODO KeywordMessageElement() " + keyword + " " + binaryObjectDescriptions);
	}

	public void visit(UnaryObjectDescription unaryObjectDescription) {
		// System.out.println("TODO UnaryObjectDescription() " + unaryObjectDescription);
	}

	public void visit(BinaryObjectDescription binaryObjectDescription) {
		// System.out.println("TODO BinaryObjectDescription() " + binaryObjectDescription);
	}

	public void visit(UnaryExpression unaryExpression) {
		// System.out.println("TODO UnaryExpression() " + unaryExpression);
	}

	public void visitEnd(UnaryExpression unaryExpression) {
		// System.out.println("TODO UnaryExpression() " + unaryExpression);
	}

	public void visit(BinaryExpression binaryExpression) {
		// System.out.println("TODO BinaryExpression() " + binaryExpression);
	}

	public void visitEnd(BinaryExpression binaryExpression) {
	}

	public void visit(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
	}

	public void visitEnd(KeywordExpression keywordExpression, String keywords, int argumentCount, int line) {
		classBytecodeWriter.keywordSend(keywords, argumentCount, line, sendToSuper);
		sendToSuper = false;
	}

	public void visit(PrimaryExpression primaryExpression) {
//		System.out.println("TODO PrimaryExpression() " + primaryExpression);
	}

	public void visit(PrimaryStatements primaryStatements) {
		System.out.println("TODO PrimaryStatements() " + primaryStatements);
	}

	public void visit(Primitive primitive, String value, int line) {
		classBytecodeWriter.callToPrimitiveByNumber(methodArgumentCount(), methodTemporariesCount(), value, line);
	}

	public void visit(Symbol symbol, String value, int line) {
//		System.out.println("TODO Symbol() " + value);
		classBytecodeWriter.callPrimitiveSymbol(value, line);
		if (insideArray())
			classBytecodeWriter.callPrimitivePutAt(symbol.index(), line);
	}

	public void visit(Array array) {
		arrayDepth++;
		classBytecodeWriter.callPrimitiveArray(array.size(), array.line());
	}

	public void visitEnd(Array array) {
		arrayDepth--;
		if (insideArray())
			classBytecodeWriter.callPrimitivePutAt(array.index(), array.line());
	}

	private boolean insideArray() {
		return arrayDepth != 0;
	}

	public void visit(Identifier identifier, String value, int line) {
		System.out.println("TODO Identifier() " + value);
	}

	public void visit(LiteralSymbol literalSymbol, String value, int line) {
//		System.out.println("LiteralSymbol() " + value);
		classBytecodeWriter.callPrimitiveSymbol(value, line);
	}

	public void visit(LiteralArray literalArray) {
//		System.out.println("LiteralArray() begin");
		arrayDepth = 0;
	}

	public void visitEnd(LiteralArray literalArray) {
//		System.out.println("LiteralArray() end");
	}

	public void visit(ArrayConstantElement arrayConstantElement) {
		System.out.println("TODO ArrayConstantElement() " + arrayConstantElement);
	}

	public void visit(CharacterConstant characterConstant, String value, int line) {
//		System.out.println("CharacterConstant() " + value);
		classBytecodeWriter.callPrimitiveCharacter(value.substring(1), line);
		if (insideArray())
			classBytecodeWriter.callPrimitivePutAt(characterConstant.index(), line);
	}

	public void visit(StringConstant stringConstant, String value, int line) {
//		System.out.println("StringConstant() " + value + " at: " + stringConstant.index());
		if (value.charAt(0) == '\'')
			classBytecodeWriter.callPrimitiveString(value.substring(1, value.length() - 1), line);
		else
			classBytecodeWriter.callPrimitiveString(value, line);
		if (insideArray())
			classBytecodeWriter.callPrimitivePutAt(stringConstant.index(), line);
	}

	public void visit(StringChunk stringChunk, String value, int line) {
		// System.out.println("TODO StringChunk() " + value);
	}

	public void visit(LiteralString literalString, String value, int line) {
//		System.out.println("LiteralString() " + value);
		if (value.charAt(0) == '\'')
			classBytecodeWriter.callPrimitiveString(value.substring(1, value.length() - 1), line);
		else
			classBytecodeWriter.callPrimitiveString(value, line);
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
		classBytecodeWriter.callPrimitiveCharacter(value.substring(1), line);
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
//		System.out.println("NumberConstant() " + value + " at: " + numberConstant.index());
		// NumberConstants happen within an array context.
		if (insideArray()) {
			classBytecodeWriter.callPrimitiveInteger(numberConstant.value(), line); // put:
			classBytecodeWriter.callPrimitivePutAt(numberConstant.index(), line);   // at:
		} else {
			classBytecodeWriter.callPrimitiveInteger(numberConstant.value(), line);
		}
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
//		System.out.println("LiteralNumber() " + value + " " + line);
		// LiteralNumbers happen outside an array context.
		classBytecodeWriter.callPrimitiveInteger(literalNumber.value(), line);
	}

	public void visit(Block block) {
//		System.out.println("Block() begin " + block + " " + blockDepth);
		blockDepth++;
		blockSequence++;
		String blockName = "B" + blockSequence;
		String fullBlockName = createFullBlockName(blockName);
		block.analyser(this);
		primitives.registerBlockToBeCompiledAs(block, fullBlockName);
		classBytecodeWriter.callPrimitiveCompileBlock(fullBlockName, block.line(), blockName, className, packageName, block.argumentCount(), isClassMethod);
	}

	private String createFullBlockName(String blockName) {
		return createFullMethodName(blockName);
	}

	public void visitEnd(Block block) {
//		System.out.println("Block() end " + block);
		blockDepth--;
	}

	public void visit(SelfReservedWord selfReservedWord, int line) {
		classBytecodeWriter.stackPushReceiver(line);
	}

	public void visit(SuperReservedWord superReservedWord, int line) {
		sendToSuper = true;
		classBytecodeWriter.stackPushSuper(line);
	}

	public void visit(TrueReservedWord selfReservedWord, int line) {
		classBytecodeWriter.stackPushTrue(line);
	}

	public void visit(FalseReservedWord selfReservedWord, int line) {
		classBytecodeWriter.stackPushFalse(line);
	}

	public void visit(NilReservedWord selfReservedWord, int line) {
		classBytecodeWriter.stackPushNil(line);
	}

	public void visit(SmalltalkReservedWord smalltalkReservedWord, int line) {
		classBytecodeWriter.stackPushSmalltalk(line);
	}

	public void visit(JvmInsn jvmInsn, int opcode, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitInsn(opcode);
	}

	public void visit(JvmLdcInsn jvmLdcInsn, String value, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitLdcInsn(value);
	}

	public void visit(JvmVarInsn jvmVarInsn, int opcode, int number, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitVarInsn(opcode, number);
	}

	public void visit(JvmMethodInsn jvmMethodInsn, int opcode, String owner, String name, String description, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitMethodInsn(opcode, owner, name, description);
	}

	public void visit(JvmFieldInsn jvmFieldInsn, int opcode, String owner, String name, String description, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitFieldInsn(opcode, owner, name, description);
	}

	public void visit(JvmTypeInsn jvmTypeInsn, int opcode, String type, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitTypeInsn(opcode, type);
	}

	public void visit(JvmLoadJavaValue jvmLoadJavaValue, int line) {
		classBytecodeWriter.loadJavaValue(line);
	}

	public void visit(JvmStoreJavaValue jvmStoreJavaValue, int line) {
		classBytecodeWriter.storeJavaValue(line);
	}

	public void visit(JvmIincInsn jvmIincInsn, int variable, int increment, int line){
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitIincInsn(variable, increment);
	}

	public void visit(JvmIntInsn jvmIntInsn, int opcode, int operand, int line) {
		classBytecodeWriter.visitLine(line);
		classBytecodeWriter.methodVisitor().visitIntInsn(opcode, operand);
	}
}
