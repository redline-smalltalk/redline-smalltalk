package st.redline.smalltalk.interpreter;

import java.util.List;

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
}
