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

import java.util.List;

public interface NodeVisitor {
	boolean continueMethodVisit();
	void visit(Program program);
	void visitEnd(Program program);
	void visit(Temporaries temporaries);
	void visitEnd(Temporaries temporaries);
	void visit(Temporary temporary, int index, String value, int line);
	void visit(VariableName variableName, String value, int line);
	void visit(Statements statements);
	void visitEnd(Statements statements);
	void visit(AnswerExpression answerExpression);
	void visit(Methods methods);
	void visitEnd(Methods methods);
	void visit(InstanceMethod instanceMethod);
	void visitEnd(InstanceMethod instanceMethod);
	void visit(ClassMethod classMethod);
	void visitEnd(ClassMethod classMethod);
	void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line);
	void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, VariableName variableName);
	void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames);
	void visit(UnarySelector unarySelector, String value, int line);
	void visit(BinarySelector binarySelector, String value, int line);
	void visit(Keyword keyword, String value, int line);
	void visit(AssignmentExpression assignmentExpression);
	void visit(SimpleExpression simpleExpression);
	void visitEnd(SimpleExpression simpleExpression);
	void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line);
	void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription);
	void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions);
	void visit(UnaryObjectDescription unaryObjectDescription);
	void visit(BinaryObjectDescription binaryObjectDescription);
	void visit(UnaryExpression unaryExpression);
	void visitEnd(UnaryExpression unaryExpression);
	void visit(BinaryExpression binaryExpression);
	void visitEnd(BinaryExpression binaryExpression);
	void visit(KeywordExpression keywordExpression, String keywords, int argumentCount, int line);
	void visitEnd(KeywordExpression keywordExpression, String keywords, int argumentCount, int line);
	void visit(PrimaryExpression primaryExpression);
	void visit(PrimaryStatements primaryStatements);
	void visit(Primitive primitive, String value, int line);
	void visit(Symbol symbol, String value, int line);
	void visit(Array array);
	void visit(Identifier identifier, String value, int line);
	void visit(LiteralSymbol literalSymbol, String value, int line);
	void visit(LiteralArray literalArray);
	void visit(ArrayConstantElement arrayConstantElement);
	void visit(CharacterConstant characterConstant, String value, int line);
	void visit(StringConstant stringConstant, String value, int line);
	void visit(StringChunk stringChunk, String value, int line);
	void visit(LiteralString literalString, String value, int line);
	void visit(LiteralCharacter literalCharacter, String value, int line);
	void visit(NumberConstant numberConstant, String value, int line);
	void visit(LiteralNumber literalNumber, String value, int line);
	void visit(Block block);
	void visitEnd(Block block);
	void visit(SelfReservedWord selfReservedWord, int line);
	void visit(SuperReservedWord superReservedWord, int line);
	void visit(TrueReservedWord selfReservedWord, int line);
	void visit(FalseReservedWord selfReservedWord, int line);
	void visit(NilReservedWord selfReservedWord, int line);
	void visit(SmalltalkReservedWord smalltalkReservedWord, int line);

	void visit(JvmInsn jvmInsn, int opcode, int line);
	void visit(JvmLdcInsn jvmLdcInsn, String literal, int line);
	void visit(JvmVarInsn jvmVarInsn, int opcode, int number, int line);
	void visit(JvmMethodInsn jvmMethodInsn, int opcode, String owner, String name, String description, int line);
	void visit(JvmFieldInsn jvmFieldInsn, int opcode, String owner, String name, String description, int line);
	void visit(JvmTypeInsn jvmTypeInsn, int opcode, String type, int line);
	void visit(JvmIincInsn jvmIincInsn, int variable, int increment, int line);
	void visit(JvmIntInsn jvmIntInsn, int opcode, int operand, int line);
	void visit(JvmLoadJavaValue jvmLoadJavaValue, int line);
	void visit(JvmStoreJavaValue jvmStoreJavaValue, int line);
}
