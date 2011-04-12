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
}
