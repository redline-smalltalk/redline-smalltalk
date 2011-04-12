package st.redline.smalltalk.interpreter;

public interface NodeVisitor {
	void visit(Program program);
	void visit(Temporaries temporaries);
	void visit(Temporary temporary, String value, int line);
	void visit(VariableName variableName, String value, int line);
	void visit(Statements statements);
	void visit(AnswerExpression answerExpression);
	void visit(Methods methods);
	void visit(InstanceMethod instanceMethod);
	void visit(ClassMethod classMethod);
	void visit(UnarySelectorMessagePattern unarySelectorMessagePattern, String value, int line);
	void visit(BinarySelectorMessagePattern binarySelectorMessagePattern, String binarySelector, int binarySelectorLine, String variableName, int variableNameLine);
	void visit(KeywordMessagePattern keywordMessagePattern);
	void visit(UnarySelector unarySelector, String value, int line);
	void visit(BinarySelector binarySelector, String value, int line);
	void visit(Keyword keyword, String value, int line);
}
