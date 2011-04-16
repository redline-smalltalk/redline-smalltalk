package st.redline.smalltalk.interpreter;

import java.util.List;

public interface NodeVisitor {
	void visit(Program program);
	void visitEnd(Program program);
	void visit(Temporaries temporaries);
	void visitEnd(Temporaries temporaries);
	void visit(Temporary temporary, int index, String value, int line);
	void visit(VariableName variableName, String value, int line);
	void visit(Statements statements);
	void visit(AnswerExpression answerExpression);
	void visit(Methods methods);
	void visit(InstanceMethod instanceMethod);
	void visit(ClassMethod classMethod);
	void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line);
	void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, String variableName, int variableNameLine);
	void visit(KeywordMessagePattern keywordMessagePattern, String keywords, int keywordLine, List<VariableName> variableNames);
	void visit(UnarySelector unarySelector, String value, int line);
	void visit(BinarySelector binarySelector, String value, int line);
	void visit(Keyword keyword, String value, int line);
	void visit(AssignmentExpression assignmentExpression);
	void visit(SimpleExpression simpleExpression);
	void visit(UnarySelectorMessageElement unarySelectorMessageElement, String value, int line);
	void visit(BinarySelectorMessageElement binarySelectorMessageElement, String value, int line, UnaryObjectDescription unaryObjectDescription);
	void visit(KeywordMessageElement keywordMessageElement, String keyword, int line, List<BinaryObjectDescription> binaryObjectDescriptions);
	void visit(UnaryObjectDescription unaryObjectDescription);
	void visit(BinaryObjectDescription binaryObjectDescription);
	void visit(UnaryExpression unaryExpression);
	void visit(BinaryExpression binaryExpression);
	void visit(KeywordExpression keywordExpression);
	void visit(PrimaryExpression primaryExpression);
	void visit(PrimaryStatements primaryStatements);
	void visit(Symbol symbol);
	void visit(Array array);
	void visit(Identifier identifier, String value, int line);
	void visit(LiteralSymbol literalSymbol);
	void visit(LiteralArray literalArray);
	void visit(ArrayConstantElement arrayConstantElement);
	void visit(NumberConstant numberConstant);
	void visit(CharacterConstant characterConstant, String value, int line);
	void visit(StringConstant stringConstant, String value, int line);
	void visit(LiteralString literalString, String value, int line);
	void visit(LiteralCharacter literalCharacter, String value, int line);
	void visit(NumberConstant numberConstant, String value, int line);
	void visit(LiteralNumber literalNumber, String value, int line);
	void visit(Block block);
}
