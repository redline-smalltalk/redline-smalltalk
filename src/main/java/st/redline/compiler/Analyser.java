/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline.compiler;

import st.redline.ProtoObject;

import java.io.File;
import java.util.List;

public class Analyser implements NodeVisitor {

	protected final String className;
	protected final String packageName;

	protected ClassBytecodeWriter classBytecodeWriter;
	private boolean sendToSuper = false;
	private AbstractMethod currentMethod;
	protected int countOfArguments;

	public Analyser(String className, String packageName) {
		this(className, packageName, 0);
	}

	public Analyser(String className, String packageName, int countOfArguments) {
		this.className = className;
		this.packageName = packageName;
		this.countOfArguments = countOfArguments;
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
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visit(Temporary temporary, int index, String value, int line) {
	}

	public void visit(VariableName variableName, String value, int line) {
		if (isTemporary(value)) {
			throw new IllegalStateException("TODO - handle temporary variable.");
		} else  {
			if (variableName.isOnLoadSideOfExpression()) {
				classBytecodeWriter.callPrimitiveVariableAt(value, line, true);
			} else {
				throw new IllegalStateException("TODO - store of variable.");
			}
		}
	}

	private boolean isTemporary(String name) {
		return false;
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
	}

	public void visitEnd(InstanceMethod instanceMethod) {
		currentMethod = null;
	}

	public void visit(ClassMethod classMethod) {
		classBytecodeWriter.callPrimitiveVariableAt(classMethod.objectName, classMethod.line(), true);
		classBytecodeWriter.callClass();
		currentMethod = classMethod;
	}

	public void visitEnd(ClassMethod classMethod) {
		currentMethod = null;
	}

	public void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line) {
		String fullMethodName = createFullMethodName(value);
		ProtoObject.registerMethodToBeCompiledAs(currentMethod, fullMethodName);
		classBytecodeWriter.callPrimitiveCompileMethod(fullMethodName, value, className, packageName, 0);
	}

	public void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName) {
		String fullMethodName = createFullMethodName(binarySelector);
		ProtoObject.registerMethodToBeCompiledAs(currentMethod, fullMethodName);
		classBytecodeWriter.callPrimitiveCompileMethod(fullMethodName, binarySelector, className, packageName, 1);
	}

	public void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames) {
		String fullMethodName = createFullMethodName(keywords);
		ProtoObject.registerMethodToBeCompiledAs(currentMethod, fullMethodName);
		classBytecodeWriter.callPrimitiveCompileMethod(fullMethodName, keywords, className, packageName, variableNames.size());
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
	}

	public void visit(AssignmentExpression assignmentExpression) {
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

	public void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line) {
	}

	public void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription) {
	}

	public void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions) {
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
		classBytecodeWriter.keywordSend(keywords, argumentCount, line, sendToSuper);
		sendToSuper = false;
	}

	public void visit(PrimaryExpression primaryExpression) {
	}

	public void visit(PrimaryStatements primaryStatements) {
	}

	public void visit(Primitive primitive, String value, int line) {
		classBytecodeWriter.callToPrimitiveByNumber(methodArgumentCount(), methodTemporariesCount(), value, line);
	}

	public void visit(Symbol symbol, String value, int line) {
	}

	public void visit(Array array) {
	}

	public void visit(Identifier identifier, String value, int line) {
	}

	public void visit(LiteralSymbol literalSymbol, String value, int line) {
		classBytecodeWriter.callPrimitiveSymbol(value, line);
	}

	public void visit(LiteralArray literalArray) {
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
		if (value.charAt(0) == '\'')
			classBytecodeWriter.callPrimitiveString(value.substring(1, value.length() - 1), line);
		else
			classBytecodeWriter.callPrimitiveString(value, line);
	}

	public void visit(LiteralCharacter literalCharacter, String value, int line) {
	}

	public void visit(NumberConstant numberConstant, String value, int line) {
	}

	public void visit(LiteralNumber literalNumber, String value, int line) {
	}

	public void visit(Block block) {
	}

	public void visitEnd(Block block) {
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
